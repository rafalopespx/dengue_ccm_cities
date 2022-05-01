rm(list=ls())
gc()

### Loading packages
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(readr)){install.packages("readr"); library(readr)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(patchwork)){install.packages("patchwork"); library(patchwork)}
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

p_seasonality<-(p1 + p2 + p3)/(p4 + p5 + p6)+
  plot_annotation(tag_levels = 'a')+
  plot_layout(guides = 'collect')
p_seasonality

ggsave(plot = p_seasonality, filename = 'Outputs/Plots/rj/plot_seasonality_all_drivers.png', 
       width = 11, 
       height = 9, 
       dpi = 300)

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


p_hist<-(p7 + p8 + p9)/(p10 + p11 + p12)+
  plot_annotation(tag_levels = 'a')+
  plot_layout(guides = 'collect')
p_hist

ggsave(plot = p_hist, filename = 'Outputs/Plots/rj/plot_hist_residuals.png', 
       width = 11,
       height = 9, 
       dpi = 300)


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

p_ts_residuals<-(p13 + p14 + p15)/(p16 + p17 +p18)+
  plot_annotation(tag_levels = 'a')+
  plot_layout(guides = 'collect')
p_ts_residuals

ggsave(plot = p_ts_residuals, filename = 'Outputs/Plots/rj/plot_ts_residuals.png', 
       width = 11, 
       height = 9, 
       dpi = 300)

## Samples surrogate

## Tmin
sample_surrogate_tmin<-sample(seasonaliy_driver$residual_temp_min)+seasonaliy_driver$temp_min_seasonality |> 
  as.data.frame() |>
  setNames(c('surrogate_temp_min'))

sample_surrogate_tmin_residual<-sample(seasonaliy_driver$residual_temp_min) |> 
  as.data.frame()
sample_surrogate_tmin_residual$date<-seasonaliy_driver$date

sample_surrogate_tmin_residual |> 
  ggplot(aes(x = date, y = `sample(seasonaliy_driver$residual_temp_min)`))+
  geom_line()
  
sample_surrogate_tmin$date<-seasonaliy_driver$date

plot_surr_tmin<-sample_surrogate_tmin |> 
  ggplot(aes(x = date, y = surrogate_temp_min))+
  geom_line()+
  geom_line(data = seasonaliy_driver, aes(x = date, y = temp_min), col = 'firebrick2')+
  theme_bw()+
  labs(title = 'Tmin surrogate')
plot_surr_tmin

ggsave(plot = plot_surr_tmin, filename = 'Outputs/Plots/rj/surr_tmin_sample.png', 
       width = 11, height = 9, dpi = 300)

## Tmean
sample_surrogate_tmean<-sample(seasonaliy_driver$residual_temp_mean)+seasonaliy_driver$temp_mean_seasonality |> 
  as.data.frame() |>
  setNames(c('surrogate_temp_mean'))

sample_surrogate_tmean$date<-seasonaliy_driver$date

plot_surr_tmean<-sample_surrogate_tmean |> 
  ggplot(aes(x = date, y = surrogate_temp_mean))+
  geom_line()+
  geom_line(data = seasonaliy_driver, aes(x = date, y = temp_mean), col = 'firebrick2')+
  theme_bw()+
  labs(title = 'Tmean surrogate')
plot_surr_tmean

ggsave(plot = plot_surr_tmean, filename = 'Outputs/Plots/rj/surr_tmean_sample.png', 
       width = 11, height = 9, dpi = 300)

## Tmax
sample_surrogate_tmax<-sample(seasonaliy_driver$residual_temp_max)+seasonaliy_driver$temp_max_seasonality |> 
  as.data.frame() |>
  setNames(c('surrogate_temp_max'))

sample_surrogate_tmax$date<-seasonaliy_driver$date

plot_surr_tmax<-sample_surrogate_tmax |> 
  ggplot(aes(x = date, y = surrogate_temp_max))+
  geom_line()+
  geom_line(data = seasonaliy_driver, aes(x = date, y = temp_max), col = 'firebrick2')+
  theme_bw()+
  labs(title = 'Tmax surrogate')
plot_surr_tmax

ggsave(plot = plot_surr_tmean, filename = 'Outputs/Plots/rj/surr_tmax_sample.png', 
       width = 11, height = 9, dpi = 300)

## Pmin
sample_surrogate_pmin<-sample(seasonaliy_driver$residual_precip_min)+seasonaliy_driver$total_precip_min_seasonality |> 
  as.data.frame() |>
  setNames(c('surrogate_precip_min'))

sample_surrogate_pmin$date<-seasonaliy_driver$date

plot_surr_pmin<-sample_surrogate_pmin |> 
  ggplot(aes(x = date, y = surrogate_precip_min))+
  geom_line()+
  geom_line(data = seasonaliy_driver, aes(x = date, y = total_precip_min), col = 'firebrick2')+
  theme_bw()+
  labs(title = 'Pmin surrogate')
plot_surr_pmin

ggsave(plot = plot_surr_pmin, filename = 'Outputs/Plots/rj/surr_pmin_sample.png', 
       width = 11, 
       height = 9, 
       dpi = 300)

## Pmean
sample_surrogate_pmean<-sample(seasonaliy_driver$residual_precip_mean)+seasonaliy_driver$total_precip_mean_seasonality |> 
  as.data.frame() |>
  setNames(c('surrogate_precip_mean'))

sample_surrogate_pmean$date<-seasonaliy_driver$date

plot_surr_pmean<-sample_surrogate_pmean |> 
  ggplot(aes(x = date, y = surrogate_precip_mean))+
  geom_line()+
  geom_line(data = seasonaliy_driver, aes(x = date, y = total_precip_mean), col = 'firebrick2')+
  theme_bw()+
  labs(title = 'Pmean surrogate')
plot_surr_pmean

ggsave(plot = plot_surr_pmean, 
       filename = 'Outputs/Plots/rj/surr_pmean_sample.png', 
       width = 11, 
       height = 9, 
       dpi = 300)

## Pmax
sample_surrogate_pmax<-sample(seasonaliy_driver$residual_precip_max)+seasonaliy_driver$total_precip_max_seasonality |> 
  as.data.frame() |>
  setNames(c('surrogate_precip_max'))

sample_surrogate_pmax$date<-seasonaliy_driver$date

plot_surr_pmax<-sample_surrogate_pmax |> 
  ggplot(aes(x = date, y = surrogate_precip_max, col = 'surrogate'))+
  geom_line()+
  geom_line(data = seasonaliy_driver, aes(x = date, y = total_precip_max, col = 'Original'))+
  theme_bw()+
  labs(title = 'Pmax surrogate')+
  scale_color_manual(name = 'series', values = c('firebrick1', 'black'))+
  theme(legend.position = 'bottom')
plot_surr_pmax

ggsave(plot = plot_surr_pmax, 
       filename = 'Outputs/Plots/rj/surr_pmax_sample.png', 
       width = 11, 
       height = 9, 
       dpi = 300)

p_surr<-(plot_surr_tmax + plot_surr_tmean + plot_surr_tmin)/
  (plot_surr_pmax + plot_surr_pmean + plot_surr_pmin)+
  plot_annotation(tag_levels = 'a')+
  plot_layout(guides = 'collect')&
  theme(legend.position = 'bottom')
p_surr

ggsave(plot = p_surr, filename = 'Outputs/Plots/rj/surrogate_sample_all_drivers.png', 
       width = 11, 
       height = 9, 
       dpi = 300)


#
