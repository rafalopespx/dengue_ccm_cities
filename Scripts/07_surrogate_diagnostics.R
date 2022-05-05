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

seasonaliy_driver<-vroom("Outputs/Tables/rj/Seasonality_complete_dengue_t2m_precip_weelky_rj.csv.xz")

seasonaliy_driver<-seasonaliy_driver %>% 
  mutate(year = year(date), 
         week = week(date), 
         month = month(date)) %>%
  mutate(residual_total_precip_mean = total_precip_mean - total_precip_mean_seasonality, 
         residual_total_precip_max = total_precip_max - total_precip_max_seasonality, 
         residual_total_precip_min = total_precip_min - total_precip_min_seasonality, 
         residual_temp_mean = temp_mean - temp_mean_seasonality, 
         residual_temp_max = temp_max - temp_max_seasonality, 
         residual_temp_min = temp_min - temp_min_seasonality)

residuals<-names(seasonaliy_driver)[grepl(names(seasonaliy_driver), pattern = "residual")]
residuals
seasonals<-names(seasonaliy_driver)[grepl(names(seasonaliy_driver), pattern = "seasonality")]
seasonals

names_driver<-c("total_precip_mean", "total_precip_max", "total_precip_min", 
                "temp_mean", "temp_max", "temp_min")

## Surrogates creation
Surr_1<-Surr_2<-vector("list", 500)

for (l in 1:500) {
  block_1<-block_2<-as.data.frame(matrix(NA, nrow = 523, ncol = 6))
  for (i in 1:6) {
    block_1[,i]<-sample(seasonaliy_driver[,residuals[i]])+
      seasonaliy_driver[,seasonals[i]]
    names(block_1)[i]<-names_driver[i]
    ## Yearly shuffle
    years<-list()
    for (k in 2010:2019) {
      years[k-2009]<-seasonaliy_driver |>
        filter(year(date) == k) |> 
        select(residuals[i])
      names(years)[k-2009]<-k
    }
    years<-sample(years)
    block_2[,i]<-c(years[[1]], years[[2]], years[[3]], years[[4]], years[[5]], 
                   years[[6]], years[[7]], years[[8]], years[[9]], years[[10]])+
      seasonaliy_driver[,seasonals[i]]
    names(block_2)[i]<-names_driver[i]
    ## Non-negative condition to precipitation series
    # if(i %in% c(1,2,3)){
    #   block_temp[,i]<-if_else(block_temp[,i] < 0, 
    #                           abs(block_temp[,i]), 
    #                           block_temp[,i])
    # }
  }
  block_1$surr<-block_2$surr<-l
  block_1$date<-block_2$date<-seasonaliy_driver$date
  Surr_1[[l]]<-block_1
  Surr_2[[l]]<-block_2
}


surr_1_df<-Surr_1 |> 
  bind_rows()

## Saving surrogates time series
vroom_write(surr_1_df, file = "Outputs/Tables/rj/surrogates_time_series.csv.xz")

surr_2_df<-Surr_2 |> 
  bind_rows()

## Saving surrogates time series
vroom_write(surr_2_df, file = "Outputs/Tables/rj/surrogates_time_series_years_shuffle.csv.xz")


boxplot_fun<-function(x, var){
  x |> 
    ggplot()+
    geom_boxplot(aes(x = date, y = {{var}}, 
                     group = date, col = "Surrogate"), 
                 show.legend = F, outlier.alpha = 0)+
    geom_line(data = seasonaliy_driver, aes(x = date, y = {{var}}, col = "Original"))+
    theme_minimal()+
    scale_x_date(date_breaks = "3 months", date_labels = "%V/%y")+
    theme(axis.text.x = element_text(angle = 90))+
    theme(legend.position = "bottom", legend.title = element_blank())
}

## Temperature Boxplot 
temp_mean_boxplot<-surr_2_df |> 
  boxplot_fun(var = temp_mean)+
  labs(y = "[ªC]", title = "Mean Temperature Surrogates")
temp_mean_boxplot

temp_min_boxplot<-surr_2_df |> 
  boxplot_fun(var = temp_min)+
  labs(y = "[ªC]", title = "Min. Temperature Surrogates")
temp_min_boxplot

temp_max_boxplot<-surr_2_df |> 
  boxplot_fun(var = temp_max)+
  labs(y = "[ªC]", title = "Max. Temperature Surrogates")
temp_max_boxplot

patch_temp<-(temp_mean_boxplot / temp_max_boxplot / temp_min_boxplot)+
  plot_annotation(tag_levels = 'a', title = "Yearly Shuffle")+
  plot_layout(guides = 'collect')&
  theme(legend.position = "bottom")
patch_temp


ggsave(plot = patch_temp, 
       filename = 'Outputs/Plots/rj/patch_surrogates_temp_year_shuffle.png', 
       width = 11, 
       height = 9,
       dpi = 300)

## Precipitation Boxplot
total_mean_precip_boxplot<-surr_2_df |> 
  boxplot_fun(var = total_precip_mean)+
  labs(y = "[mm]", title = "Total Mean Precipitation Surrogates")
total_mean_precip_boxplot

total_min_precip_boxplot<-surr_2_df |> 
  boxplot_fun(var = total_precip_min)+
  labs(y = "[mm]", title = "Total Min. Precipitation Surrogates")
total_min_precip_boxplot

total_max_precip_boxplot<-surr_2_df |> 
  boxplot_fun(var = total_precip_max)+
  labs(y = "[mm]", title = "Total Max. Precipitation Surrogates")
total_max_precip_boxplot

patch_precip<-(total_mean_precip_boxplot / total_max_precip_boxplot / total_min_precip_boxplot)+
  plot_annotation(tag_levels = 'a', title = "Yearly Shuffle")+
  plot_layout(guides = 'collect')&
  theme(legend.position = "bottom")
patch_precip

ggsave(plot = patch_precip, 
       filename = 'Outputs/Plots/rj/patch_surrogates_precip_negative_year_shuffle.png', 
       width = 11, 
       height = 9, 
       dpi = 300)


## Final Patch
patch_final<-(temp_mean_boxplot / temp_max_boxplot / temp_min_boxplot |
  total_mean_precip_boxplot / total_max_precip_boxplot / total_min_precip_boxplot)+
  plot_annotation(tag_levels = 'a', title = "Yearly Shuffle")+
  plot_layout(guides = 'collect')&
  theme(legend.position = "bottom")
patch_final

ggsave(plot = patch_final, 
       filename = 'Outputs/Plots/rj/patch_final_surrogates.png', 
       width = 11, 
       height = 9, 
       dpi = 300)

## Diagnostics
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




  