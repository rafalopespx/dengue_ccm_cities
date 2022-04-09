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

dengue_t2m_rio<-vroom('Data/Work_data/dengue_t2m_precip_weelky_rj.csv.xz')

rj_plot<-dengue_t2m_rio %>% 
  ggplot(aes(x = week, y = Cases, col = year(week)))+
  geom_line()+
  theme_minimal()+
  labs(x = "Date", y = "N Cases", title = "Rio de Janeiro")+
  scale_colour_viridis_c(option = "viridis")+
  scale_x_date(date_breaks = "5 months", date_labels = "%b/%y")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))
rj_plot

# sp_plot<-dengue_t2m_sp %>% 
#   ggplot(aes(x = week, y = Cases, col = year(week)))+
#   geom_line()+
#   theme_minimal()+
#   labs(x = "Date", y = "N Cases", title = "Sao Paulo")+
#   scale_colour_viridis_c(option = "viridis")+
#   scale_x_date(date_breaks = "5 months", date_labels = "%b/%y")+
#   theme(legend.position = "none", axis.text.x = element_text(angle = 90))
# sp_plot

ggsave(plot = rj_plot, 
       filename = "Outputs/Plots/rj/cases_rj.png", 
       width = 9, 
       height = 7, 
       dpi = 300)

rj_temp_plot<-dengue_t2m_rio %>% 
  ggplot(aes(x = week, y = temp_mean, col = year(week)))+
  geom_line()+
  theme_minimal()+
  labs(x = "Date", y = "Temp Mean", title = "Rio de Janeiro")+
  scale_colour_viridis_c(option = "viridis")+
  scale_x_date(date_breaks = "5 months", date_labels = "%b/%y")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))
rj_temp_plot

ggsave(plot = rj_temp_plot, 
       filename = "Outputs/Plots/rj/temp_rj.png", 
       width = 9, 
       height = 7, 
       dpi = 300)

rj_precip_plot<-dengue_t2m_rio %>% 
  ggplot(aes(x = week, y = total_precip_mean, col = year(week)))+
  geom_line()+
  theme_minimal()+
  labs(x = "Date", y = "Total Preciptation Mean", title = "Rio de Janeiro")+
  scale_colour_viridis_c(option = "viridis")+
  scale_x_date(date_breaks = "5 months", date_labels = "%b/%y")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))
rj_precip_plot

ggsave(plot = rj_precip_plot, 
       filename = "Outputs/Plots/rj/precip_rj.png", 
       width = 9, 
       height = 7, 
       dpi = 300)