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
if(!require(patchwork)){install.packages("patchwork"); library(patchwork)}
if(!require(colorspace)){install.packages("colorspace"); library(colorspace)}
if(!require(viridis)){install.packages("viridis"); library(viridis)}
if(!require(ggblanket)){install.packages("ggblanket"); library(ggblanket)}

more_col<-"precip_min_once_add_tmin"

## Laoding drivers optimal csv
# drivers_coef_opt_52<-vroom(paste0("Outputs/Tables/rj/yearly_shuffle_drivers_tp_", 
#                                   52,
#                                   "_add_",
#                                   more_col,
#                                   "_coef_opt_theta_mae.csv.xz"))

drivers_coef_opt_17<-vroom(paste0("Outputs/Tables/rj/yearly_shuffle_drivers_tp_", 
                                  17,
                                  "_add_",
                                  more_col,
                                  "_coef_opt_theta_mae.csv.xz"))

rj_drivers<-vroom("Data/dengue_t2m_precip_weelky_rj.csv.xz") |> 
  select(week, temp_mean, temp_max, temp_min, total_precip_mean, total_precip_max, total_precip_min) |> 
  rename(date = week)

drivers_coef_opt_17<-drivers_coef_opt_17 |> 
  left_join(rj_drivers) |> 
  mutate(month = factor(month(date, label = T, abbr = T)))

# drivers_coef_opt_52<-drivers_coef_opt_52 |> 
#   left_join(rj_drivers)


coef_plot<-function(x, xvar, yvar, colors = NULL, size = NULL){
  x %>% 
    ggplot(aes(x = {{xvar}}, y = {{yvar}}, 
               col = {{colors}}, 
               size = {{size}}))+
    geom_point()+
    geom_rug(sides = "b")+
    theme_bw()
}

paleta_cores<-c(rev(viridis(6)), viridis(6))

## Coefficients
## Until 17 weeks
coef_precip_max_tp_17<-drivers_coef_opt_17 |> 
  coef_plot(xvar = total_precip_max2, 
            yvar = `∂total_precip_max2/∂cases`, 
            colors = month)+
  scale_color_manual(values = paleta_cores, name = "Month")+
  theme(legend.position = "bottom")+
  labs(title = "Effects of Maximum Precipitation on Cases", 
       subtitle = "by theta for min. MAE",
       y = expression(paste(partialdiff,"Cases","/",partialdiff,"Precip"[max]," (t - 2)")), 
       x = expression(paste("Precip"[max],"(t - 2)")))
coef_precip_max_tp_17

ggsave(filename = paste0("Outputs/Plots/rj/coef_precip_max_tp_17", 
                         "_add_",
                         more_col,
                         ".png"), 
       plot = coef_precip_max_tp_17, 
       width = 9, 
       height = 7, 
       dpi = 300)

coef_precip_min_tp_17<-drivers_coef_opt_17 |> 
  coef_plot(xvar = total_precip_min7, 
            yvar = `∂total_precip_min7/∂cases`, 
            colors = month)+
  labs(title = "Effects of Minimum Precipitation on Cases", 
       subtitle = "by theta for min. MAE")+
  scale_color_manual(values = paleta_cores, name = "Month")+
  theme(legend.position = "bottom")+
  labs(y = expression(paste(partialdiff,"Cases","/",partialdiff,"Precip"[min]," (t - 7)")), 
       x = expression(paste("Precip"[min], "(t - 7)")))
coef_precip_min_tp_17

ggsave(filename = paste0("Outputs/Plots/rj/coef_precip_min_tp_17", 
                         "_add_",
                         more_col,
                         ".png"), 
       plot = coef_precip_min_tp_17, 
       width = 9, 
       height = 7, 
       dpi = 300)

coef_add_tp_17<-drivers_coef_opt_17 |> 
  coef_plot(xvar = Casest1, 
            yvar = `∂Casest1/∂cases`, 
            colors = month)+
  labs(title = "Effects of Cases lagged 1 week on Cases", 
       subtitle = "by theta for min. MAE")+
  scale_color_manual(values = paleta_cores, name = "Month")+
  theme(legend.position = "bottom")+
  labs(y = expression(paste(partialdiff,"Cases","/",partialdiff,"Cases (t - 1)")), 
       x = expression(paste("Cases (t - 1)")))
coef_add_tp_17

ggsave(filename = paste0("Outputs/Plots/rj/coef_casest1_tp_17", 
                         "_add_",
                         more_col,
                         ".png"), 
       plot = coef_add_tp_17, 
       width = 9, 
       height = 7, 
       dpi = 300)

coef_add_tp_17_2<-drivers_coef_opt_17 |> 
  coef_plot(xvar = Casest2, 
            yvar = `∂Casest2/∂cases`, 
            colors = month)+
  labs(title = "Effects of Cases lagged 2 weeks on Cases", 
       subtitle = "by theta for min. MAE")+
  scale_color_manual(values = paleta_cores, name = "Month")+
  theme(legend.position = "bottom")+
  labs(y = expression(paste(partialdiff,"Cases","/",partialdiff," Cases (t - 2)")), 
       x = expression(paste("Cases (t - 2)")))
coef_add_tp_17_2

ggsave(filename = paste0("Outputs/Plots/rj/coef_casest2_tp_17", 
                         "_add_",
                         more_col,
                         ".png"), 
       plot = coef_add_tp_17_2, 
       width = 9, 
       height = 7, 
       dpi = 300)

coef_add_tp_17_3<-drivers_coef_opt_17 |> 
  coef_plot(xvar = Casest3, 
            yvar = `∂Casest3/∂cases`, 
            colors = month)+
  labs(title = "Effects of Cases lagged 3 weeks on Cases", 
       subtitle = "by theta for min. MAE")+
  scale_color_manual(values = paleta_cores, name = "Month")+
  theme(legend.position = "bottom")+
  labs(y = expression(paste(partialdiff,"Cases","/",partialdiff," Cases (t - 3)")), 
       x = expression(paste("Cases (t - 3)")))
coef_add_tp_17_3

ggsave(filename = paste0("Outputs/Plots/rj/coef_casest3_tp_17", 
                         "_add_",
                         more_col,
                         ".png"), 
       plot = coef_add_tp_17_3, 
       width = 9, 
       height = 7, 
       dpi = 300)

patch_precip_tp_17<-(coef_precip_min_tp_17)|(coef_add_tp_17 / coef_add_tp_17_2 / coef_add_tp_17_3)+
  plot_layout(guides = "collect")+
  plot_annotation(title = "Force of Interaction", 
                  tag_levels = "a", 
                  theme = theme_classic())&
  theme(legend.position = "bottom")
patch_precip_tp_17

ggsave(filename = paste0("Outputs/Plots/rj/patchwork_coef_", more_col, "_tp_17.png"),
       plot = patch_precip_tp_17, 
       width = 9, 
       height = 7, 
       dpi = 300)

patch_precip_tp_17<-(coef_precip_max_tp_17)|(coef_add_tp_17 / coef_add_tp_17_2 / coef_add_tp_17_3)+
  plot_layout(guides = "collect")+
  plot_annotation(title = "Force of Interaction", 
                  tag_levels = "a", 
                  theme = theme_classic())&
  theme(legend.position = "bottom")
patch_precip_tp_17

ggsave(filename = paste0("Outputs/Plots/rj/patchwork_coef_", more_col, "_tp_17.png"),
       plot = patch_precip_tp_17, 
       width = 9, 
       height = 7, 
       dpi = 300)


#