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


drivers_coef_opt_mae<-vroom("Outputs/Tables/rj/drivers_coef_opt_theta_mae.csv.xz")
drivers_coef_opt_rmse<-vroom("Outputs/Tables/rj/drivers_coef_opt_theta_rmse.csv.xz")

coef_plot<-function(x, xvar, yvar, colors = NULL){
  x %>% 
    ggplot(aes(x = {{xvar}}, y = {{yvar}}, col = {{colors}}))+
    geom_point()+
    geom_rug(sides = "b")+
    theme_bw()
}

## Coefficients
coef_tmin<-coef_plot(x = drivers_coef_opt_mae, 
                     xvar = temp_min2, 
                     yvar = `∂temp_min2/∂cases`, colors = NULL)+
  labs(x = "Minimum Temperature [ºC], lagged 2 weeks", 
       title = "Effects of Minimum Temperature on Cases", 
       subtitle = "by theta for min. MAE")
coef_tmin

ggsave(filename = "Outputs/Plots/rj/dTmin2_Tmin.png", 
       plot = coef_tmin, 
       width = 9, 
       height = 7, 
       dpi = 300)

coef_tmin_colors<-drivers_coef_opt_mae %>% 
  coef_plot(xvar = temp_min2, yvar = `∂temp_min2/∂cases`, colors = month(date - 2))+
  labs(x = "Minimum Temperature [ºC], lagged 2 weeks", 
       title = "Effects of Minimum Temperature on Cases")+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  theme(legend.position = "bottom")
coef_tmin_colors

# marginal_tmin_colors<-ggExtra::ggMarginal(
#   p = coef_tmin_colors,
#   type = 'density',
#   margins = 'x',
#   size = 5,
#   colour = 'black',
#   fill = 'gray',
#   xparams = list(alpha = 0)
# )
# marginal_tmin_colors

ggsave(filename = "Outputs/Plots/rj/dTmin2_Tmin_colors.png", 
       plot = coef_tmin, 
       width = 9, 
       height = 7, 
       dpi = 300)

coef_tmin_ts<-drivers_coef_opt_mae %>% 
  mutate(year = year(date-52), 
         week = week(date-52), 
         month = month(date-52)) %>%
  ggplot(aes(x = date, y = `∂temp_min52/∂cases`, col = month))+
  geom_line()+
  geom_rug(sides = "b")+
  theme_bw()+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  theme(legend.position = "bottom")
coef_tmin_ts

coef_precip_min<-drivers_coef_opt_mae %>% 
  coef_plot(xvar = total_precip_min7, yvar = `∂total_precip_min7/∂cases`)+
  labs(x = "Minimum Preciptation [m], lagged 7 week", 
       title = "Effects of Minimum Preciptation on Cases")
coef_precip_min

ggsave(filename = "Outputs/Plots/rj/dprecip_min25.png", 
       plot = coef_precip_min, 
       width = 9, 
       height = 7, 
       dpi = 300)

coef_precip_min_colors<-drivers_coef_opt_mae %>% 
  mutate(year = year(date-25), 
         week = week(date-25), 
         month = month(date-25)) %>% 
  filter(year < 2020) %>% 
  ggplot(aes(x = total_precip_min25, 
             y = `∂total_precip_min25/∂cases`, col = month))+
  geom_point()+
  geom_rug(sides = "b")+
  theme_bw()+
  labs(x = "Minimum Preciptation [m], lagged 25 weeks", 
       title = "Effects of Minimum Preciptation on Cases")+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  theme(legend.position = "bottom")
coef_precip_min_colors

ggsave(filename = "Outputs/Plots/rj/dprecip_min7_colors.png", 
       plot = coef_precip_min_colors, 
       width = 9, 
       height = 7, 
       dpi = 300)

coef_precip_min_ts<-drivers_coef_opt_mae %>% 
  mutate(year = year(date-25), 
         week = week(date-25), 
         month = month(date-25)) %>% 
  ggplot(aes(x = date, y = `∂total_precip_min25/∂cases`, col = month))+
  geom_line()+
  theme_bw()+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  theme(legend.position = "bottom")
coef_precip_min_ts

coef_tmean<-drivers_coef_opt_mae %>% 
  coef_plot(xvar = temp_mean2, yvar = `∂temp_mean2/∂cases`)+
  labs(x = "Mean Temperature [ºC], lagged 2 weeks", 
       title = "Effects of Mean Temperature on Cases")
coef_tmean

ggsave(filename = "Outputs/Plots/rj/dTmean2_Tmean.png", 
       plot = coef_tmean, 
       width = 9, 
       height = 7, 
       dpi = 300)

coef_tmean_colors<-drivers_coef_opt_mae %>% 
  coef_plot(xvar = temp_mean2, yvar = `∂temp_mean2/∂cases`, colors = month(date - 2))+
  labs(x = "Mean Temperature [ºC], lagged 2 weeks", 
       title = "Effects of Mean Temperature on Cases")+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  theme(legend.position = "bottom")
coef_tmean_colors

ggsave(filename = "Outputs/Plots/rj/dTmean2_Tmean_colors.png", 
       plot = coef_tmean_colors, 
       width = 9, 
       height = 7, 
       dpi = 300)

coef_tmean_ts<-drivers_coef_opt_mae %>% 
  mutate(year = year(date-52), 
         week = week(date-52), 
         month = month(date-52)) %>% 
  ggplot(aes(x = date, y = `∂temp_mean52/∂cases`, col = month))+
  geom_line()+
  theme_bw()+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  theme(legend.position = "bottom")
coef_tmean_ts

cases_ts<-drivers_coef_opt_mae %>% 
  mutate(year = year(date), 
         week = week(date), 
         month = month(date)) %>% 
  ggplot(aes(x = date, y = Cases, col = week))+
  geom_line()+
  # geom_rug(sides = "b")+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = "bottom")+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  scale_x_date(date_breaks = "1 year")
cases_ts

library(patchwork)

cases_ts_df<-drivers_coef_opt_mae %>% 
  select(Cases, date)
tmin_ts_df<-drivers_coef_opt_mae %>% 
  select(`∂temp_min52/∂cases`, date) %>% 
  mutate(date = date - 7*52)
tmean_ts_df<-drivers_coef_opt_mae %>% 
  select(`∂temp_mean52/∂cases`, date) %>% 
  mutate(date = date - 7*52)
precip_ts_df<-drivers_coef_opt_mae %>% 
  select(`∂total_precip_min25/∂cases`, date) %>% 
  mutate(date = date - 7*25)

ts_coef_colors<-cases_ts_df %>% 
  left_join(tmin_ts_df) %>% 
  left_join(tmean_ts_df) %>% 
  left_join(precip_ts_df)%>% 
  mutate(year = year(date), 
         week = week(date), 
         month = month(date))
## Cases ts
cases_ts_new<-ts_coef_colors %>% 
  ggplot()+
  geom_line(aes(x = date, y = Cases, col = month))+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        legend.position = "bottom")+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")
cases_ts_new
  
cases_ts_new_zoom<-ts_coef_colors %>% 
  filter(date > "2011-09-01" & date < "2013-09-01") %>% 
  ggplot()+
  # geom_line(aes(x = date, y = Cases, col = month))+
  geom_line(aes(x = date, y = `∂temp_min52/∂cases`, col = month))+
  geom_line(aes(x = date, y = `∂temp_mean52/∂cases`, col = month))+
  geom_line(aes(x = date, y = `∂total_precip_min25/∂cases`, col = month))+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        legend.position = "bottom")+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")
cases_ts_new_zoom

cases_ts_zoom<-ts_coef_colors %>% 
  filter(date > "2011-09-01" & date < "2013-09-01") %>% 
  ggplot(aes(x = date, y = Cases, col = month))+
  geom_line()+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = "bottom")+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")
cases_ts_zoom

## tmin TS
tmin_ts_new<-ts_coef_colors %>% 
  ggplot(aes(x = date, y = `∂temp_min52/∂cases`, col = month))+
  geom_line()+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = "bottom")+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")
tmin_ts_new  

tmin_ts_zoom<-ts_coef_colors %>% 
  filter(date > "2011-09-01" & date < "2013-09-01") %>% 
  ggplot(aes(x = date, y = `∂temp_min52/∂cases`, col = month))+
  geom_line()+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = "bottom")+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  scale_x_date(date_breaks = "3 weeks", date_labels = "%V/%y")
tmin_ts_zoom

## Precip TS
precip_ts_new<-ts_coef_colors %>% 
  # filter(date > "2011-09-01" & date < "2013-09-01") %>% 
  ggplot(aes(x = date, y = `∂total_precip_min25/∂cases`, col = month))+
  geom_line()+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = "bottom")+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")
precip_ts_new

precip_ts_zoom<-ts_coef_colors %>% 
  filter(date > "2011-09-01" & date < "2013-09-01") %>% 
  ggplot(aes(x = date, y = `∂total_precip_min25/∂cases`, col = month))+
  geom_line()+
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = "bottom")+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  scale_x_date(date_breaks = "3 weeks", date_labels = "%V/%y")
precip_ts_zoom

## Tmean TS
tmean_ts_new<-ts_coef_colors %>% 
  # filter(date > "2011-09-01" & date < "2013-09-01") %>% 
  ggplot(aes(x = date, y = `∂temp_mean52/∂cases`, col = month))+
  geom_line()+
  theme_bw()+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")+
  theme(axis.text.x = element_text(angle = 90), 
        axis.title.x = element_blank(), 
        legend.position = "bottom")
tmean_ts_new

tmean_ts_zoom<-ts_coef_colors %>% 
  filter(date > "2011-09-01" & date < "2013-09-01") %>%
  ggplot(aes(x = date, y = `∂temp_mean52/∂cases`, col = month))+
  geom_line()+
  theme_bw()+
  scale_color_fermenter(type = "seq", palette = "PuOr")+
  scale_x_date(date_breaks = "3 weeks", date_labels = "%V/%y")+
  theme(axis.text.x = element_text(angle = 90), 
        axis.title.x = element_blank(), 
        legend.position = "bottom")
tmean_ts_zoom

ts_coef<-(cases_ts_new/tmin_ts_new/precip_ts_new/tmean_ts_new)+
  plot_layout(guides = "collect")+
  plot_annotation(title = "Interaction Strength of Drivers on Cases", 
                  tag_levels = "a", theme = theme_minimal())&
  theme(legend.position = "bottom")
ts_coef

ggsave(plot = ts_coef, 
       filename = "Outputs/Plots/rj/ts_colors_months.png", 
       width = 11,
       height = 9, 
       dpi = 300)

ts_coef_zoom<-(cases_ts_zoom/cases_ts_new_zoom)+
  plot_layout(guides = "collect")+
  plot_annotation(title = "Interaction Strength of Drivers on Cases", 
                  tag_levels = "a", theme = theme_minimal())&
  theme(legend.position = "bottom")
ts_coef_zoom

ggsave(plot = ts_coef_zoom, 
       filename = "Outputs/Plots/rj/ts_colors_month_zoom.png", 
       width = 11,
       height = 9, 
       dpi = 300)

coef_patchwork<-(coef_tmin_colors | coef_tmean_colors / coef_precip_min_colors)+
  plot_layout(guides = "collect")+
  plot_annotation(title = "Coefficients vs Drivers", tag_levels = "a", theme = theme_classic())&
  theme(legend.position = "bottom")
coef_patchwork

ggsave(filename = "Outputs/Plots/rj/effects_all_drivers_months.png", 
       plot = coef_patchwork, 
       width = 11, 
       height = 9, 
       dpi = 300)

#