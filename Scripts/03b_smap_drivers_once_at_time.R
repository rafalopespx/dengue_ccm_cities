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

maxE<-EmbedDimension(dataFrame = dengue_t2m_rio, 
                     lib = "1 523", 
                     pred = "1 523", 
                     maxE = 20, 
                     target = "Cases",
                     columns = "Cases",
                     showPlot = T)

maxE<-maxE[which.max(maxE$rho),]$E

dengue_embed<-Embed(dataFrame = dengue_t2m_rio, E = maxE, columns = "Cases", tau = 1) |> 
  setNames(c("Cases", str_c("Casest", 1:(maxE-1))))

dengue_t2m_rio<-data.frame(dengue_t2m_rio, dengue_embed[,-1])

tp<-17
more_col<-"precip_max_once"

driver_surr<-vroom(paste0('Outputs/Tables/rj/yearly_shuffle_', tp,'_driver_surr.csv.xz')) %>% 
  filter(sig == T) %>% 
  mutate(driver = case_when(driver == "Mean Temperature" ~ "temp_mean", 
                            driver == "Max. Temperature" ~ "temp_max", 
                            driver == "Min. Temperature" ~ "temp_min", 
                            driver == "Mean Precipitation" ~ "total_precip_mean", 
                            driver == "Max. Precipitation" ~ "total_precip_max", 
                            driver == "Min. Precipitation" ~ "total_precip_min"))

## Selecting from the driver data, only significant for the original time series, 
lag_select<-driver_surr %>% 
  filter(sig == T)%>% 
  select(tp, rho, driver)%>%
  group_by(driver) %>% 
  arrange(desc(rho)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(tp_abs = abs(tp))

driver_once<-driver_surr |> 
  filter(driver == "total_precip_max")

series_cutted<-ccm_coef_cutter(driver_data = driver_once, 
                               original = dengue_t2m_rio, 
                               add_col = c("Casest1", "Casest2", "Casest3"),
                               K = 0)
names_smap<-colnames(series_cutted$Norm_block)[-1]

# Saving series cutted
vroom_write(series_cutted$Norm_block, 
            file = paste0("Outputs/Tables/rj/normlized_series_cut_tp_", 
                          tp, 
                          "_add_", 
                          more_col, 
                          ".csv.xz"))
vroom_write(series_cutted$Series, 
            file = paste0("Outputs/Tables/rj/series_cut_tp_", 
                          tp, 
                          "_add_", 
                          more_col, 
                          ".csv.xz"))

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
            file = paste0('Outputs/Tables/rj/yealry_shuffle_tp_', 
                          tp, 
                          "_add_", 
                          more_col, 
                          '_theta_mae_rmse.csv.xz'))

coef_matrix<-coef_matrix %>% 
  bind_rows()

vroom_write(x = coef_matrix, 
            file = paste0('Outputs/Tables/rj/yealry_shuffle_tp_', 
                          tp, 
                          "_add_", 
                          more_col, 
                          '_coef_matrix.csv.xz'))

drivers_coef<-drivers_coef %>% 
  bind_rows()

vroom_write(x = drivers_coef, 
            file = paste0('Outputs/Tables/rj/yealry_shuffle_tp_', 
                          tp, 
                          "_add_", 
                          more_col, 
                          '_drivers_coef.csv.xz'))

#