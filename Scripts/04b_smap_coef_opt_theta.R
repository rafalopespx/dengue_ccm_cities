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
source("Scripts/functions/coef_smap.R")

dengue_t2m_rio<-vroom('Data/dengue_t2m_precip_weelky_rj.csv.xz')

tp<-17
# more_col<-"temp_min"

theta_find<-vroom(paste0("Outputs/Tables/rj/yealry_shuffle_tp_", 
                         tp,
                         # "_add_", 
                         # more_col,
                         "_theta_mae_rmse.csv.xz"))

min_mae<-theta_find$mae[which.min(theta_find$mae)]
theta_min_mae<-theta_find$theta[which.min(theta_find$mae)]

theta_min_plot<-theta_find %>% 
  ggplot(aes(x = theta, y = mae, col = "MAE"))+
  geom_line()+
  geom_line(aes(x = theta, y = rmse, col = "RMSE"))+
  theme_bw()+
  geom_text(data = theta_find[which.min(theta_find$mae), ], 
            aes(x = theta, 
                y = mae+.01, 
                label = theta))+
  geom_text(data = theta_find[which.min(theta_find$rmse), ], 
            aes(x = theta, 
                y = rmse+.01, 
                label = theta))+
  labs(x = expression(theta), y = "", 
       title = "Comparison between MAE and RMSE", 
       subtitle = "for the Prediction and Observations")+
  theme(legend.position = "bottom", legend.title = element_blank())
theta_min_plot

ggsave(filename = paste0('Outputs/Plots/rj/yearly_shuffle_tp_', 
                         tp,
                         # "_add_", 
                         # more_col,
                         '_theta_min_mae_rmse.png'), 
       width = 11, 
       height = 9, 
       dpi = 300)

### Optimal Theta
theta_opt_mae<-theta_find$theta[which.min(theta_find$mae)]

## Loading Series
series_cutted<-list()

series_cutted$Norm_block<-vroom(paste0("Outputs/Tables/rj/normlized_series_cut_tp_", 
                                       tp,
                                       # "_add_", 
                                       # more_col,
                                       ".csv.xz"))
series_cutted$Series<-vroom(paste0("Outputs/Tables/rj/series_cut_tp_", 
                                   tp, 
                                   # "_add_", 
                                   # more_col,
                                   ".csv.xz"))

names_smap<-colnames(series_cutted$Norm_block)[-1]
max_tp<-max(parse_number(names_smap))
length_rj<-nrow(dengue_t2m_rio)

drivers_coef_opt_mae<-coef_fun(df_N = series_cutted$Norm_block, 
                              df = series_cutted$Series, 
                              theta = theta_min_mae, 
                              cols = names_smap, 
                              target = 'cases', 
                              max_tp = max_tp, 
                              via = "smap", 
                              cutoff = length_rj)


vroom_write(drivers_coef_opt_mae, 
            file = paste0('Outputs/Tables/rj/yearly_shuffle_drivers_tp_', 
                          tp,
                          "_add_", 
                          more_col,
                          '_coef_opt_theta_mae.csv.xz'))

# drivers_coef_opt_rmse<-coef_fun(df_N = series_cutted$Norm_block, 
#                                 df = series_cutted$Series, 
#                                 theta = theta_opt_rmse, 
#                                 cols = names_smap, 
#                                 target = 'cases', 
#                                 max_tp = max_tp)
# 
# vroom_write(drivers_coef_opt_rmse, 
#             file = 'Outputs/Tables/rj/drivers_coef_opt_theta_rmse.csv.xz')

#