rm(list=ls())
gc()

### Loading packages
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(readr)){install.packages("readr"); library(readr)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(ggblanket)){install.packages("ggblanket"); library(ggblanket)}
if(!require(patchwork)){install.packages("patchwork"); library(patchwork)}

dengue_t2m_rio<-vroom('Data/dengue_t2m_precip_weelky_rj.csv.xz') |> 
  setNames(c("Week", "Mean Precipitation", "Max. Precipitation", "Min. Precipitation", 
             "Mean Temperature", "Max. Temperature", "Min. Temperature", "Cases"))

timeseries<-colnames(dengue_t2m_rio[,-1])

acf_list<-list()
plot_list<-list()

for (i in timeseries) {
  acf_list[[i]]<-acf(dengue_t2m_rio |> 
                    pull(var = i), 
                  plot = FALSE, lag.max = 52)$acf |> 
    as.data.frame() |> 
    mutate(week = row_number()) |> 
    rename(ACF = V1)
  
  plot_list[[i]]<-acf_list[[i]] |> 
    gg_col(x = week, y = ACF, 
           col = ACF, 
           subtitle = i,
           x_title = "Lag weeks", 
           y_title = "Correlation")
}

acf_list<-acf_list |> 
  bind_rows(.id = "var")


acf_list |> 
  gg_col(x = week, y = ACF, 
         col = ACF, 
         title = "AutoCorrelations", 
         x_title = "Lag weeks", 
         y_title = "Correlation", facet = var)

## Patchwork

patch_work_acf<-((plot_list[[7]])| (plot_list[[1]] / plot_list[[2]] / plot_list[[3]]) |
                                     (plot_list[[4]] / plot_list[[5]] / plot_list[[6]]))+
  plot_annotation(title = "ACF for Cases and Climate Drivers", tag_levels = "a", tag_sep = " ", tag_suffix = ") ")+
  plot_layout(guides = "collect")
patch_work_acf

ggsave(filename = "Outputs/Plots/rj/acf_cases_drivers.png", 
       plot = patch_work_acf, 
       width = 11, 
       height = 9, 
       dpi = 300)

#