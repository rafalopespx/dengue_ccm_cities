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


min_mae<-theta_find$mae[which.min(theta_find$mae)]
theta_min_mae<-theta_find$theta[min_mae]
min_rmse<-theta_find$rmse[which.min(theta_find$rmse)]
theta_min_rmse<-theta_find$theta[min_rmse]

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

ggsave(filename = 'Outputs/Plots/rj/theta_min_mae_rmse.png', width = 11, height = 9, dpi = 300)

### Optimal Theta
theta_opt_mae<-theta_find$theta[which.min(theta_find$mae)]
theta_opt_rmse<-theta_find$theta[which.min(theta_find$rmse)]

coef_fun<-function(df_N, df, theta, cols, target, max_tp){
  coef_series_opt<-block_lnlp(df_N,
                              theta=theta, ## Finding Theta
                              columns = cols,
                              target_column = target,
                              method = 's-map',
                              tp = 0, 
                              save_smap_coefficients = T)
  
  coef_matrix_opt<-coef_series_opt$smap_coefficients[[1]]
  
  drivers_coef_opt<-data.frame(df, 
                               coef_matrix_opt[,3:length(coef_matrix_opt)])%>% 
    setNames(c("Cases", cols, 
               names(coef_matrix_opt)[3:length(coef_matrix_opt)]))%>% 
    mutate(date = dengue_t2m_rio$week[(max_tp+1):(length_rj - max_tp)], 
           theta = theta)
  
  return(drivers_coef_opt)
  
}

coef_series_opt<-block_lnlp(series_cutted$Norm_block,
                            theta=theta_opt_mae, ## Finding Theta
                            columns = names_smap,
                            target_column = 'cases',
                            method = 's-map',
                            tp = 0, 
                            save_smap_coefficients = T)

coef_matrix_opt<-coef_series_opt_mae$smap_coefficients[[1]]

drivers_coef_opt<-data.frame(series_cutted$Series, coef_matrix_opt[,3:length(coef_matrix_opt)])%>% 
  setNames(c("Cases", names_smap, 
             names(coef_matrix_opt)[3:length(coef_matrix_opt)]))%>% 
  mutate(date = dengue_t2m_rio$week[(max_tp+1):(length_rj - max_tp)], 
         theta = theta_opt_mae)

drivers_coef_opt_mae<-coef_fun(df_N = series_cutted$Norm_block, 
                               df = series_cutted$Series, 
                               theta = theta_opt_mae, 
                               cols = names_smap, 
                               target = 'cases', 
                               max_tp = max_tp)

vroom_write(drivers_coef_opt_mae, 
            file = 'Outputs/Tables/rj/drivers_coef_opt_theta_mae.csv.xz')

drivers_coef_opt_rmse<-coef_fun(df_N = series_cutted$Norm_block, 
                                df = series_cutted$Series, 
                                theta = theta_opt_rmse, 
                                cols = names_smap, 
                                target = 'cases', 
                                max_tp = max_tp)

vroom_write(drivers_coef_opt_rmse, 
            file = 'Outputs/Tables/rj/drivers_coef_opt_theta_rmse.csv.xz')