rm(list=ls())
gc()

### Loading packages
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(tidyr)){install.packages("tidyr"); library(tidyr)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(gtsummary)){install.packages("gtsummary"); library(gtsummary)}
if(!require(webshot)){install.packages("webshot"); library(webshot)}

# setwd("~/Desktop/Dengue_severity/")

source("../central_covid/sex-differences-mortality/Functions/functions.R")
# source("Functions/functions.R")

dengue_sinan<-vroom("Data/Dengue_data/all_final_aggregated_sin_pri_2000_2020.csv.xz")
names(dengue_sinan)<-tolower(names(dengue_sinan))

table_ready<-dengue_sinan %>% 
  filter(!is.na(idade) & idade <= 120 &
           !is.na(cs_sexo) & cs_sexo == "I")%>% 
  dplyr::mutate(age = idade,
                age_cat = as.factor(case_when(idade >= 0 & idade <= 1 ~ "0-1",
                                              idade > 1 & idade <= 9 ~ "1-9",
                                              idade >= 10 & idade <=17 ~ "10-17",
                                              idade >= 18 & idade <= 39 ~ "18-39", 
                                              idade >= 40 & idade <= 59 ~ "40-59", 
                                              idade >= 60 & idade <= 79 ~ "60-79", 
                                              idade >= 80 & idade <= 120 ~ "80+", 
                                              TRUE ~ idade)),
                time_notific = dt_sin_pri - dt_notific, 
                sex = as.factor(case_when(cs_sexo == "M" ~ "Male", 
                                          cs_sexo == "F" ~ "Female", 
                                          TRUE ~ cs_sexo)),
                race = as.factor(case_when(cs_raca == 1 ~ "White", 
                                           cs_raca == 2 ~ "Black", 
                                           cs_raca == 3 ~ "Brown", 
                                           cs_raca == 4 ~ "Yellow", 
                                           cs_raca == 5 ~ "Indigenous", 
                                           TRUE ~ cs_raca)),
                outcome = as.factor(case_when(evolucao == 0 ~ "Discharged", 
                                              evolucao == 1 ~ "Death", 
                                              TRUE ~ evolucao)), 
                sg_uf = str_sub(id_mn_resi, 1,2), 
                abbrev_state = sg_uf)
table_ready<-regiao(table_ready, english = T)

vroom::vroom_write(table_ready, file = "Outputs/Tables/table_ready.csv")

# Dengue Table by Sex
dengue_table<-table_ready %>% 
  dplyr::select(age, age_cat, race, race_3_bin, race_bin, region, sex, 
                # capital_metro_res, capital_metro_mov,
                time_interna, outcome) %>% 
  tbl_summary(by = sex,
              missing = "ifany",
              missing_text = "(Missing)",
              percent = "column",
              type = all_dichotomous() ~ "categorical",
              label = list(age ~ "Age",
                           age_cat ~ "Age in Categories",
                           race ~ "Self-Reported Race",
                           race_3_bin ~ "Self-Reported Race \n 3 Categories",
                           race_bin ~ "Self-Reported Race \n Binary",
                           region ~ "Region",
                           time_interna ~ "Time of Hospitalization",
                           outcome ~ "Outcome"
                           # capital_metro_res ~ "Municipality of Residence", 
                           # capital_metro_mov ~ "Health Facility Municipality"
              )
  )%>% 
  modify_header(label ~ "**Characteristics Variables**") %>%
  add_overall() %>% 
  add_n() %>% 
  bold_labels() %>% 
  modify_caption("Table 1. Raw data Dengue Hospitalizations, by Sex")

dengue_table

dengue_table %>% 
  as_gt() %>%
  gt::gtsave(filename = "Outputs/Tables/dengue_aih_summary.png")

dengue_table %>% 
  as_gt() %>%
  gt::gtsave(filename = "Outputs/Tables/dengue_aih_summary.html")

dengue_table %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path = "Outputs/Tables/dengue_aih_summary.docx")

# Dengue Table by Region
dengue_table_region<-table_ready %>% 
  dplyr::select(age, age_cat, race, 
                # race_3_bin, race_bin, 
                region, sex, 
                # capital_metro_res, capital_metro_mov,
                time_notific, outcome) %>% 
  mutate(region = factor(region,
                         levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) %>%
  tbl_summary(by = region,
              missing = "ifany",
              missing_text = "(Missing)",
              percent = "column",
              type = all_dichotomous() ~ "categorical",
              label = list(age ~ "Age",
                           age_cat ~ "Age in Categories",
                           race ~ "Self-Reported Race",
                           # race_3_bin ~ "Self-Reported Race \n 3 Categories",
                           # race_bin ~ "Self-Reported Race \n Binary",
                           sex ~ "Sexo",
                           time_notific ~ "Time of Hospitalization",
                           # capital_metro_res ~ "Municipality of Residence", 
                           # capital_metro_mov ~ "Health Facility Municipality",
                           outcome ~ "Outcome"
              )
  )%>% 
  modify_header(label ~ "**Characteristics Variables**") %>%
  add_overall() %>% 
  add_n() %>% 
  bold_labels() %>% 
  modify_caption("Table 1. Raw data Dengue Cases, 2010-2019s, by Regions")

dengue_table_region

dengue_table_region %>% 
  as_gt() %>%
  gt::gtsave(filename = "Outputs/Tables/dengue_region_aih_summary.png")

dengue_table_region %>% 
  as_gt() %>%
  gt::gtsave(filename = "Outputs/Tables/dengue_region_aih_summary.html")

dengue_table_region %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path = "Outputs/Tables/dengue_region_aih_summary.docx")

# Dengue Table by Capitals/Non-Capitals
dengue_table_capital_res<-table_ready %>% 
  dplyr::select(age, age_cat, race, race_3_bin, race_bin, region, sex, capital_metro_res, capital_metro_mov,
                time_interna, outcome) %>% 
  tbl_summary(by = capital_metro_res,
              missing = "always",
              missing_text = "(Missing)",
              percent = "column",
              type = all_dichotomous() ~ "categorical",
              label = list(age ~ "Age",
                           age_cat ~ "Age in Categories",
                           race ~ "Self-Reported Race",
                           race_3_bin ~ "Self-Reported Race \n 3 Categories",
                           race_bin ~ "Self-Reported Race \n Binary",
                           sex ~ "Sexo",
                           time_interna ~ "Time of Hospitalization",
                           outcome ~ "Outcome", 
                           region ~ "Region",
                           capital_metro_mov ~ "Health Facility Municipality")
  )%>% 
  modify_header(label ~ "**Characteristics Variables**") %>%
  add_overall() %>% 
  add_n() %>% 
  bold_labels() %>% 
  modify_caption("Table 1. Raw data Dengue Hospitalizations, by Municipality of Residence")

dengue_table_capital_res

dengue_table_capital_res %>% 
  as_gt() %>%
  gt::gtsave(filename = "Outputs/Tables/dengue_capital_res_aih_summary.png")

dengue_table_capital_res %>% 
  as_gt() %>%
  gt::gtsave(filename = "Outputs/Tables/dengue_capital_res_aih_summary.html")

dengue_table_capital_res %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path = "Outputs/Tables/dengue_capital_res_aih_summary.docx")

# Dengue Table by Capitals/Non-Capitals by municipality of health facility
dengue_table_capital_mov<-table_ready %>% 
  dplyr::select(age, age_cat, race, race_3_bin, race_bin, region, sex, capital_metro_res, capital_metro_mov,
                time_interna, outcome) %>% 
  tbl_summary(by = capital_metro_mov,
              missing = "always",
              missing_text = "(Missing)",
              percent = "column",
              type = all_dichotomous() ~ "categorical",
              label = list(age ~ "Age",
                           age_cat ~ "Age in Categories",
                           race ~ "Self-Reported Race",
                           race_3_bin ~ "Self-Reported Race \n 3 Categories",
                           race_bin ~ "Self-Reported Race \n Binary",
                           sex ~ "Sexo",
                           time_interna ~ "Time of Hospitalization",
                           outcome ~ "Outcome", 
                           region ~ "Region",
                           capital_metro_res ~ "Municipality Residence")
  )%>% 
  modify_header(label ~ "**Characteristics Variables**") %>%
  add_overall() %>% 
  add_n() %>% 
  bold_labels() %>% 
  modify_caption("Table 1. Raw data Dengue Hospitalizations, by Health Facility Municipality")

dengue_table_capital_mov

dengue_table_capital_mov %>% 
  as_gt() %>%
  gt::gtsave(filename = "Outputs/Tables/dengue_capital_mov_aih_summary.png")

dengue_table_capital_mov %>% 
  as_gt() %>%
  gt::gtsave(filename = "Outputs/Tables/dengue_capital_mov_aih_summary.html")

dengue_table_capital_mov %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path = "Outputs/Tables/dengue_capital_mov_aih_summary.docx")

# Cases Ranking
br_muni<-read_csv("Outputs/Tables/municipalities_br.csv")
## Yearly
dengue_summary_yearly<-table_ready %>% 
  group_by(munic_res, ano_inter) %>% 
  summarise(N=n()) %>% 
  arrange(desc(N)) %>% 
  mutate(code_muni = munic_res)
dengue_summary_yearly<-dengue_summary_yearly %>% 
  left_join(br_muni, by = "code_muni")
dengue_summary_yearly<-dengue_summary_yearly %>%
  select(name_muni, munic_res, ano_inter, N) %>% 
  setNames(c("Municipality", "IBGE Code", "Year", "Number of Hospitalization"))

flx_table_dengue_yearly <- flextable::flextable(dengue_summary_yearly)
flx_table_dengue_yearly<-flextable::set_header_labels(
  flx_table_dengue_yearly,
  name_muni = "Municipality", 
  munic_res = "Code Municipality",
  ano_inter = "Year", 
  N = "Number of Hospitalizations")
flx_table_dengue_yearly<-flextable::theme_zebra(flx_table_dengue_yearly) 

flx_table_dengue_yearly %>% flextable::save_as_html(path = "Outputs/Tables/ranking_yearly_cases.html")

# flx_table_dengue_yearly %>% flextable::save_as_docx(path = "Outputs/Tables/ranking_yearly_cases.docx")

## Total
dengue_summary<-table_ready %>% 
  group_by(munic_res) %>% 
  summarise(N=n()) %>% 
  arrange(desc(N)) %>% 
  mutate(percent = N/sum(N), 
         percent_cumsum = cumsum(percent), 
         code_muni = munic_res,
         n_cidades = row_number())
dengue_summary<-dengue_summary %>% 
  left_join(br_muni, by = "code_muni")
dengue_summary<-dengue_summary %>% 
  select(name_muni, munic_res, N, percent_cumsum, n_cidades) %>% 
  setNames(c("Municipality", "IBGE Code", "Number of Hospitalizations", "Cumulative Percentual", "Number of Cities"))

flx_table_dengue <- flextable::flextable(dengue_summary)
flx_table_dengue<-flextable::set_header_labels(
  flx_table_dengue,
  name_muni = "Municipality", 
  munic_res = "Code Municipality",
  N = "Number of Hospitalizations", 
  percent_cumsum = "Cumulative Percentual", 
  n_cidades = "Number of Cities")
flx_table_dengue<-flextable::theme_zebra(flx_table_dengue)

flx_table_dengue %>% flextable::save_as_html(path = "Outputs/Tables/ranking_cases.html")

# flx_table_dengue %>% flextable::save_as_docx(path = "Outputs/Tables/ranking_cases.docx")
