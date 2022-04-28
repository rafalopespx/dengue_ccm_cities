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

residuals<-names(seasonaliy_driver)[grepl(names(seasonaliy_driver), pattern = "residual")]
seasonals<-names(seasonaliy_driver)[grepl(names(seasonaliy_driver), pattern = "seasonality")]

## Surrogates creation
Surr_df<-vector("list", 500)

for (l in 1:500) {
  block_temp<-as.data.frame(matrix(NA, nrow = 523, ncol = 6))
  for (i in 1:6) {
    block_temp[,i]<-slice_sample(seasonaliy_driver[,residuals[i]], n = 523, replace = T)+
      seasonaliy_driver[,seasonals[i]]
  }
  block_temp$surr<-l
  Surr_df[[l]]<-block_temp
}


Surr_temp_min<-
  