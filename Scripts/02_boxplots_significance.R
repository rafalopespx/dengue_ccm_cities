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

boxplot_files<-list.files(path = "Outputs/Tables/rj/", pattern = "yearly_shuffle_Boxplot_tp=", full.names = T)
max_tp_vec<-max(tp_vec)
path<-"yearly_shuffle"

boxplot_files<-lapply(boxplot_files, function(x){
  # tp<-str_extract(x, "[.-]\\d+|0")
  x<-vroom(x) %>% 
    rename(series = ...7, 
           tp = ...8) 
})

boxplot_files<-boxplot_files %>% 
  bind_rows() %>% 
  mutate(series = if_else(series != "Original", "Surrogate", series)) %>% 
  arrange(desc(tp))

vroom_write(boxplot_files, 
            file = paste0("Outputs/Tables/rj/", path,"_rho_values_surr_original_rj.csv"))

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
  setNames(c("Mean Precipitation", "Max. Precipitation", "Min. Precipitation", 
             "Mean Temperature", "Max. Temperature", "Min. Temperature", "series", "tp")) %>% 
  pivot_longer(names_to = "driver", values_to = "rho", cols = -c(tp,series))%>% 
  group_by(tp, driver) %>% 
  mutate(sig = sig_fun(rho, 0.05), 
         sig_val = sig_fun_q(rho)) %>% 
  group_by(tp, driver, series, sig) %>% 
  arrange(tp)

## Untill which maximum tp we go
tp_vec<--c(0:52)

## by two different df
surr_stats<-surr_quants<-vector("list", length = length(tp_vec))

for (l in 1:length(tp_vec)) {
  surr_df<-boxplot_df %>% 
    filter(series != "Original") %>% 
    filter(tp == tp_vec[l])
  
  surr_stats[[l]]<-surr_df %>% 
    # mutate(tp = as.numeric(tp)) %>% 
    # pivot_longer(names_to = "driver", values_to = "rho", cols = -c(tp, series)) %>% 
    group_by(tp,driver) %>% 
    arrange(tp)
  
  surr_quants[[l]]<-surr_df %>% 
    # mutate(tp = as.numeric(tp)) %>% 
    # pivot_longer(names_to = "driver", values_to = "rho", cols = -c(tp, series)) %>% 
    group_by(tp,driver) %>% 
    summarise(rhoq50 = quantile(rho, probs = 0.5, na.rm = T),
              rhoq05 = quantile(rho, probs = 0.05, na.rm = T),
              rhoq95 = quantile(rho, probs = 0.95, na.rm = T)) %>%
    arrange(tp)
}  

surr_stats<-surr_stats %>%
  bind_rows()

surr_quants<-surr_quants %>% 
  bind_rows()

driver_stats<-vector("list", length = length(tp_vec))

for (l in 1:length(tp_vec)) {
  driver_df<-boxplot_df %>% 
    filter(series == "Original") %>% 
    filter(tp == tp_vec[l])
  
  driver_stats[[l]]<-driver_df %>% 
    # mutate(tp = as.numeric(tp)) %>% 
    # pivot_longer(names_to = "driver", values_to = "rho", cols = -tp) %>% 
    group_by(tp,driver) %>% 
    arrange(tp)
}

driver_stats<-driver_stats %>% 
  bind_rows()

boxplot_driver_surr<-surr_stats %>% 
  mutate(driver_f = factor(driver, 
                         levels = c("Max. Temperature", "Mean Temperature", "Min. Temperature", 
                                    "Max. Precipitation", "Mean Precipitation", "Min. Precipitation"))) %>% 
  ggplot()+
  geom_boxplot(aes(x = tp, 
                 # ymin = min(rho), ymax = max(rho), 
                 y = rho, 
                      group = tp,
                   col = "Surrogate"), 
               outlier.alpha = 0.25)+
  geom_point(data = driver_stats |> 
               mutate(driver_f = factor(driver,
                                        levels = c("Max. Temperature", "Mean Temperature", "Min. Temperature",
                                                   "Max. Precipitation", "Mean Precipitation", "Min. Precipitation"))),
             aes(x = tp, y = rho, col = "Original"),
             size = 2, shape = 19)+
  facet_wrap(.~driver_f, scales = "free_y")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x = "Lag (weeks)", y = expression(paste("Cross-Mapping skill (", rho, ")")))+
  scale_color_manual(values = c("dark orange", "black"), name = "")
boxplot_driver_surr

ggsave(boxplot_driver_surr, 
       filename = paste0("Outputs/Plots/rj/", path, "_boxplot_",abs(min(tp_vec)),"_weeks_tp_type.png"), 
       width = 9, 
       height = 7, 
       dpi = 300)

driver_surr<-driver_stats %>% 
  mutate(sig = replace_na(sig, FALSE))%>% 
  group_by(tp, driver, series) %>% 
  arrange(tp)

vroom_write(x = driver_surr, 
            file = paste0('Outputs/Tables/rj/', path, '_', abs(min(tp_vec)),'_driver_surr.csv.xz'))

boxplot_driver<-driver_surr %>% 
  mutate(driver_f = factor(driver, 
                           levels = c("Max. Temperature", "Mean Temperature", "Min. Temperature", 
                                      "Max. Precipitation", "Mean Precipitation", "Min. Precipitation"))) %>% 
  ggplot()+
  geom_point(aes(x = tp, y = rho, 
                 col = ifelse(sig, "significative", "non-significative"), 
                 shape = sig), size = 3)+
  facet_wrap(.~driver_f, scales = "free_y")+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x = "Lag (weeks)", y = expression(paste("Cross-Mapping skill (", rho, ")")))+
  scale_color_manual(values = c("black", "firebrick1"), name = "")
boxplot_driver

ggsave(boxplot_driver, 
       filename = paste0('Outputs/Plots/rj/', path, '_boxplot_drivers_tp_sig_',abs(min(tp_vec)),'_weeks.png'), 
       width = 9, 
       height = 7, 
       dpi = 300)

#