rm(list=ls())
gc()

### Loading packages
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(readr)){install.packages("readr"); library(readr)}
if(!require(vroom)){install.packages("vroom"); library(vroom)}
if(!require(rEDM)){install.packages("rEDM"); library(rEDM)}
if(!require(foreach)){install.packages("foreach"); library(foreach)}
if(!require(doParallel)){install.packages("doParallel"); library(doParallel)}
if(!require(parallel)){install.packages("parallel"); library(parallel)}

source("Scripts/CCM_functions_pipeline/SeasonalSplines.R")
source("Scripts/CCM_functions_pipeline/CCMCoefficients.R")
source("Scripts/CCM_functions_pipeline/CCMSplines.R")
source("Scripts/CCM_functions_pipeline/make_pred_nozero.R")

dengue_t2m_rio<-vroom('Data/dengue_t2m_precip_weelky_rj.csv.xz')

seasonality_rj<-SeasonalSplines(DataFile = 'Data/dengue_t2m_precip_weelky_rj.csv.xz', 
                                Index_var = 1:6, 
                                complete = T, 
                                silent = T, 
                                plot = T, 
                                save.plot = T)

ccmsplines_rj<-vector("list", 6)
## Da pra paralelizar
registerDoParallel(6)  # use multicore, set to the number of our cores
foreach (i=0:52) %dopar% {
# for(i in 0:10){
  ccmsplines_rj[[i+1]]<-CCMSplines(OriginalFile='Data/dengue_t2m_precip_weelky_rj.csv.xz',
                                   SeasonalityFile='Outputs/Tables/rj/Seasonality_dengue_t2m_precip_weelky_rj.csv.xz',
                                   SurrFile='Outputs/Tables/rj/surrogates_time_series_years_shuffle.csv.xz',
                                   residual_shuffle = F,
                                   DateCol=1,
                                   LibraryCol=8,
                                   TimeForPred=-i,
                                   MaxE=10,
                                   tau = 1,
                                   NSurr=500,
                                   Normalize=T,
                                   alpha=0.05, 
                                   silent = T)
  print(paste0("Finished for tp equal to ", -i))
  gc()
}
stopImplicitCluster()
