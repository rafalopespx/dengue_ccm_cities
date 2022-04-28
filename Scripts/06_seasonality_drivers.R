rm(list=ls())
gc()

### Loading packages
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(readr)){install.packages("readr"); library(readr)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(rEDM)){install.packages("rEDM"); library(rEDM)}
if(!require(ggExtra)){install.packages("ggExtra"); library(ggExtra)}

seasonaliy_driver<-vroom("Outputs/Tables/rj/Seasonality_complete_dengue_t2m_precip_weelky_rj.csv.xz")

seasonaliy_driver<-seasonaliy_driver %>% 
  mutate(year = year(date), 
         week = week(date), 
         month = month(date)) %>%
  mutate(residual_precip_mean = total_precip_mean - total_precip_mean_seasonality, 
         residual_precip_min = total_precip_min - total_precip_min_seasonality, 
         residual_precip_max = total_precip_max - total_precip_max_seasonality, 
         residual_temp_min = temp_min - temp_min_seasonality, 
         residual_temp_mean = temp_mean - temp_mean_seasonality, 
         residual_temp_max = temp_max - temp_max_seasonality)


plot_ts<-function(x, var){
  x %>% 
    ggplot(aes(x = date, y = {{var}}))+
    geom_line()+
    theme_minimal()+
    scale_x_date(date_breaks = "3 months", date_labels = "%V/%y")+
    theme(axis.text.x = element_text(angle = 90))
}

plot_hist<-function(x, var, bins){
  x %>% 
    ggplot(aes(x = {{var}}))+
    geom_histogram(bins = bins)+
    theme_minimal()
}


## Seasonality plots
p1<-seasonaliy_driver |>  
  plot_ts(var = total_precip_mean_seasonality)+
  labs(x = "Date", y = "mm", title = "Mean Precipitation Seasonality")
p1
p2<-seasonaliy_driver |>  
  plot_ts(var = total_precip_max_seasonality)+
  labs(x = "Date", y = "mm", title = "Max. Precipitation Seasonality")
p2
p3<-seasonaliy_driver |>  
  plot_ts(var = total_precip_min_seasonality)+
  labs(x = "Date", y = "mm", title = "Min. Precipitation Seasonality")
p3
p4<-seasonaliy_driver |>  
  plot_ts(var = temp_mean_seasonality)+
  labs(x = "Date", y = "", title = "Mean Temperature Seasonality")
p4
p5<-seasonaliy_driver |>  
  plot_ts(var = temp_min_seasonality)+
  labs(x = "Date", y = "", title = "Min Temperature Seasonality")
p5
p6<-seasonaliy_driver |>  
  plot_ts(var = temp_max_seasonality)+
  labs(x = "Date", y = "", title = "Max Temperature Seasonality")
p6


## Histogram plots
p7<-seasonaliy_driver |> 
  plot_hist(var = residual_precip_mean, bins = 100)+
  labs(x = "mm",  y = "", title = "Residuals of Mean precipitation")
p7
p8<-seasonaliy_driver |> 
  plot_hist(var = residual_precip_min, bins = 100)+
  labs(x = "mm",  y = "", title = "Residuals of Min precipitation")
p8
p9<-seasonaliy_driver |> 
  plot_hist(var = residual_precip_max, bins = 100)+
  labs(x = "mm",  y = "", title = "Residuals of Max precipitation")
p9
p10<-seasonaliy_driver |> 
  plot_hist(var = residual_temp_mean, bins = 100)+
  labs(x = "[ºC]", y = "", title = "Residuals of Mean Temperature")
p10
p11<-seasonaliy_driver |> 
  plot_hist(var = residual_temp_min, bins = 100)+
  labs(x = "[ºC]", y = "", title = "Residuals of Min Temperature")
p11
p12<-seasonaliy_driver |> 
  plot_hist(var = residual_temp_max,  bins = 100)+
  labs(x = "[ºC]", y = "", title = "Residuals of Max Temperature")
p12


## Residuals Time series
p13<-seasonaliy_driver |> 
  plot_ts(var = residual_precip_mean)+
  labs(x = "date", y = "mm", title = "Residuals of Mean Precipitation")
p13
p14<-seasonaliy_driver |> 
  plot_ts(var = residual_precip_min)+
  labs(x = "date", y = "mm", title = "Residuals of Min Precipitation")
p14
p15<-seasonaliy_driver |> 
  plot_ts(var = residual_precip_max)+
  labs(x = "date", y = "mm", title = "Residuals of Max Precipitation")
p15
p16<-seasonaliy_driver |> 
  plot_ts(var = residual_temp_mean)+
  labs(x = "date", y = "[ªC]", title = "Residuals of Mean Temperature")
p16
p17<-seasonaliy_driver |> 
  plot_ts(var = residual_temp_min)+
  labs(x = "date", y = "[ªC]", title = "Residuals of Min Temperature")
p17
p18<-seasonaliy_driver |> 
  plot_ts(var = residual_temp_max)+
  labs(x = "date", y = "[ªC]", title = "Residuals of Max Temperature")
p18


#
