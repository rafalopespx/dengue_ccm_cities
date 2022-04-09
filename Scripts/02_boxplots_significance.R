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


boxplot_files<-list.files(path = "Outputs/Tables/rj/", pattern = "Boxplot_tp=", full.names = T)

boxplot_files<-lapply(boxplot_files, function(x){
  tp<-str_extract(x, "[.-]\\d+|0")
  x<-vroom(x) %>% 
    mutate(tp = tp)
})

boxplot_files<-boxplot_files %>% 
  bind_rows()

sig_fun<-function(x, alpha){
  (sum(x[1]>x[-1]) / (length(x)-1))>(1-alpha)
}

sig_fun_q<-function(x){
  (sum(x[1]>x[-1]) / (length(x)-1))
}

type_fun<-function(x){
  ifelse(x[1], "Original", "Surrogate")
}

levels_lab<-c("Max. Temp.", 
              "Mean Temp.",
              "Min. Temp.",
              "Max. Precip.", 
              "Mean Precip.", 
              "Min. Precip.")

## Boxplot with significance over tp
boxplot_df<-boxplot_files %>% 
  mutate(tp = as.numeric(tp)) %>% 
  pivot_longer(names_to = "driver", values_to = "rho", cols = -tp)%>% 
  group_by(tp, driver) %>% 
  mutate(sig = sig_fun(rho, 0.05), 
         sig_val = sig_fun_q(rho)) %>% 
  group_by(tp, driver, sig) %>% 
  arrange(tp)

## by two different df
surr_stats<-surr_quants<-vector("list", length = length(tp_vec))

for (l in 1:length(tp_vec)) {
  surr_df<-boxplot_files %>% 
    filter(tp == tp_vec[l])
  
  surr_stats[[l]]<-surr_df[-1,] %>% 
    mutate(tp = as.numeric(tp)) %>% 
    pivot_longer(names_to = "driver", values_to = "rho", cols = -tp) %>% 
    group_by(tp,driver) %>% 
    arrange(tp)
  
  surr_quants<-surr_df[-1,] %>% 
    mutate(tp = as.numeric(tp)) %>% 
    pivot_longer(names_to = "driver", values_to = "rho", cols = -tp) %>% 
    group_by(tp,driver) %>% 
    summarise(rhoq50 = quantile(rho, probs = 0.5, na.rm = T),
              rhoq05 = quantile(rho, probs = 0.05, na.rm = T),
              rhoq95 = quantile(rho, probs = 0.95, na.rm = T)) %>%
    arrange(tp)
}  

surr_stats<-surr_stats %>%
  bind_rows() %>% 
  mutate(type = "surrogate")

surr_quants<-surr_quants %>% 
  bind_rows() %>% 
  mutate(type = "surrogate")

driver_stats<-vector("list", length = length(tp_vec))

for (l in 1:length(tp_vec)) {
  driver_df<-boxplot_files %>% 
    filter(tp == tp_vec[l])
  
  driver_stats[[l]]<-driver_df[1,] %>% 
    mutate(tp = as.numeric(tp)) %>% 
    pivot_longer(names_to = "driver", values_to = "rho", cols = -tp) %>% 
    group_by(tp,driver) %>% 
    arrange(tp)
}

driver_stats<-driver_stats %>% 
  bind_rows() %>% 
  mutate(type = "original")

boxplot_driver_surr<-surr_stats %>% 
  ggplot()+
  geom_boxplot(aes(x = tp, y = rho, group = tp, 
                   col = "Surrogate"), 
               outlier.alpha = 0.25)+
  geom_point(data = driver_stats, aes(x = tp, y = rho, col = "Original"), 
             size = 2, shape = )+
  facet_wrap(~driver, scales = "free_y")+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("dark orange", "black"), name = "")
boxplot_driver_surr

ggsave(boxplot_driver_surr, 
       filename = "Outputs/Plots/rj/boxplot_all_tp_type.png", 
       width = 9, 
       height = 7, 
       dpi = 300)

driver_surr<-driver_stats %>% 
  left_join(boxplot_df %>% 
              filter(sig == T))%>% 
  mutate(sig = replace_na(sig, FALSE))%>% 
  group_by(tp, driver, type) %>% 
  arrange(tp)

boxplot_driver<-driver_surr %>% 
  ggplot()+
  geom_point(aes(x = tp, y = rho, 
                 col = ifelse(sig, "significative", "non-significative"), 
                 shape = sig), size = 3)+
  facet_wrap(~driver, scales = "free_y")+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("black", "firebrick1"), name = "")
boxplot_driver

ggsave(boxplot_driver, 
       filename = "Outputs/Plots/rj/boxplot_drivers_tp_sig.png", 
       width = 9, 
       height = 7, 
       dpi = 300)
