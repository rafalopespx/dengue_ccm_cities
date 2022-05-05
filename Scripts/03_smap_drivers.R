rm(list=ls())
gc()

### Loading packages
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(readr)){install.packages("readr"); library(readr)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(rEDM)){install.packages("rEDM"); library(rEDM)}

source("Scripts/CCM_functions_pipeline/SeasonalSplines.R")
source("Scripts/CCM_functions_pipeline/CCMCoefficients.R")
source("Scripts/CCM_functions_pipeline/CCMSplines.R")
source("Scripts/CCM_functions_pipeline/make_pred_nozero.R")
source("Scripts/functions/ccm_coef_cutter.R")

dengue_t2m_rio<-vroom('Data/dengue_t2m_precip_weelky_rj.csv.xz')

driver_surr<-vroom("Outputs/Tables/rj/driver_surr.csv.xz") %>% 
  filter(sig == T) %>% 
  mutate(driver = case_when(driver == "Mean Temperature" ~ "temp_mean", 
                            driver == "Max. Temperature" ~ "temp_max", 
                            driver == "Min. Temperature" ~ "temp_min", 
                            driver == "Mean Precipitation" ~ "total_precip_mean", 
                            driver == "Max. Precipitation" ~ "total_precip_max", 
                            driver == "Min. Precipitation" ~ "total_precip_min"))

series_cutted<-ccm_coef_cutter(driver_data = driver_surr, 
                               original = dengue_t2m_rio, 
                               K = 0)
names_smap<-colnames(series_cutted$Norm_block)[-1]

theta_vec<-seq(.1,10, by = .1)
drivers_coef<-vector("list", length(theta_vec))
coef_matrix<-vector("list", length = length(theta_vec))
theta_find<-vector("list", length = length(theta_vec))
max_tp<-series_cutted$max_tp
length_rj<-nrow(dengue_t2m_rio)

for (i in 1:length(theta_vec)) {
  coef_series<-block_lnlp(series_cutted$Norm_block,
                          theta=theta_vec[i], ## Finding Theta
                          columns = names_smap,
                          target_column = 'cases',
                          method = 's-map',
                          tp = 0, 
                          save_smap_coefficients = T)
  
  coef_matrix[[i]]<-coef_series$smap_coefficients[[1]]
  coef_matrix[[i]]$theta<-theta_vec[i]
  theta_find[[i]]<-c(theta = unique(coef_series$theta), 
                     mae = unique(coef_series$mae), 
                     rmse = unique(coef_series$rmse))
  
  drivers_coef[[i]]<-data.frame(series_cutted$Series, coef_matrix[[i]][,3:length(coef_matrix[[i]])])%>% 
    setNames(c("Cases", names_smap, 
               names(coef_matrix[[i]])[3:length(coef_matrix[[i]])]))%>% 
    mutate(date = dengue_t2m_rio$week[(max_tp+1):(length_rj - max_tp)], 
           theta = theta_vec[i])
  
  # names_saved<-paste0('Outputs/Tables/rj/interaction_coef_theta_', i, '.csv.xz')
  # vroom_write(file = names_saved, 
  #             x = drivers_coef)
}

theta_find<-theta_find %>% 
  bind_rows()

vroom_write(x = theta_find, 
            file = 'Outputs/Tables/rj/theta_mae_rmse.csv.xz')

coef_matrix<-coef_matrix %>% 
  bind_rows()

vroom_write(x = coef_matrix, 
            file = 'Outputs/Tables/rj/coef_matrix.csv.xz')

drivers_coef<-drivers_coef %>% 
  bind_rows()

vroom_write(x = drivers_coef, 
            file = 'Outputs/Tables/rj/drivers_coef.csv.xz')

#