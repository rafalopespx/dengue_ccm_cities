# This function obtains the convergent-cross mapping between seasonal
#               drivers (target variables) and one affected variable, 
#               e.g. malaria cases, (Library variable).
# please see: vignette("rEDM-tutorial")
# input
# OriginalFile: File with data to be analysed
# SeasonalityFile: File with seasonality data
# DateCol: Column with date
# LibraryCol: Variable column to be cross-mapped from
# TimeForPred: Time for prediction
# MaxE: Maximum embedding to be used in optimal search
# NSurr: Number of surrogate series
# Normalize: Normalize variables (T or F)
# alpha: significance for statistical test
# output
# Boxplot_tp=[TimeForPred].eps: plot with ccm analysis for the original data 
#                               and surrogates (boxplot). Red asterisks
# Boxplot_tp=[TimeForPred].csv: Spreadsheet with ccm values for [NSurr] surrogate series and 
#                               for the original series (first line of spreadsheet).
CCMSplines<-function(OriginalFile='../data/DataMalariaTartagalCCM.csv',
                     SeasonalityFile='Seasonality_DataMalariaTartagalCCM.csv',
                     DateCol=1,
                     LibraryCol=2,
                     TimeForPred=1,
                     tau,
                     MaxE=10,
                     NSurr=500,
                     Normalize=T,
                     alpha=0.05,
                     silent = F){
  
  require(vroom)
  require(lubridate)
  require(tidyverse)
  require(viridis)
  require(rEDM)
  
  Original<-vroom(OriginalFile)
  Seasonality<-vroom(SeasonalityFile)
  AllVariablesNames<-names(Original)
  DriversNames<-AllVariablesNames[-c(DateCol,LibraryCol)]
  Drivers<-seq(1:ncol(Original))[-c(DateCol,LibraryCol)]
  Target<-Original[,LibraryCol]
  
  if (Normalize==T){
    NormData<-Original[,-DateCol]
    nomes<-names(NormData)
    NormData<-NormData %>% 
      mutate_at(.vars = nomes, 
                .funs = function(x){
                  x<-(x-mean(x))/sd(x)
                })
    Original[,-DateCol]<-NormData
  }
  
  lib_ccm<-c(1,NROW(Original))
  
  # if(!silent){
    if(missing(tau)){
      tau<-1
      warning("Tau missing, set up to tau=1")
    }
    
    if(TimeForPred<=0){
      libSize_max<-function(N, tau, E, tp){
        A <- (abs( tau ) * (E-1))
        B <- ((tp + 1))
        return(nrow(N) - A + B - 2)
      }
      warning("lib_ccm max, set up to libSize_max = N - A + B - 2")
    } else if(TimeForPred>0) {
      libSize_max<-lib_ccm<-function(N, tau, E, tp){
        A <- (abs( tau ) * (E-1))
        B <- ((tp + 1))
        return(nrow(N) - A)
      }
      warning("lib_ccm max, set up to libSize_max = N - A")
    } #else {
      # stop("TimeForPred can not be 0!")
    # }
  # }
  
  residuals_df<-Original %>% 
    mutate(doy = yday(week)) %>% 
    left_join(Seasonality, by = "doy", suffix = c("", "_seasonality"))
  
  Residuals<-as.data.frame(matrix(NA,NROW(Original),length(Drivers)))
  RhoSurr<-matrix(NA,(NSurr+1),length(Drivers)+1)
  # Surr<-c()
  signf<-matrix(NA,1,length(Drivers))
  
  # count<-1
  for (j in Drivers){
    print(paste0(DriversNames[j-1], " started!"))
    
    ## Assessing the driver
    vars<-residuals_df[grep(DriversNames[j-1], names(residuals_df))]
    ## subtracting the seasonality to the generate the residuals of the Driver
    Residuals[,j-1]<-(vars[,1]-vars[,2])
    ## Creating objects to keep the Surrogating test
    block_temp<-as.data.frame(matrix(NA,NROW(Original),2))
    E_star<-(matrix(NA,1,NSurr))
    
    ## Paralelizar
    ## Looping over the Surrogates series
    for (i in 1:NSurr){
      
      ## Target series and random sampled Residual with the seasonality pattern added
      block_temp[,1]<-Target
      block_temp[,2]<-sample(Residuals[,j-1])+vars[,2]
      
      ## Non-negative condition to precipitation series
      if(DriversNames[j-1] %in% c("total_precip_mean", "total_precip_min", "total_precip_max")){
        block_temp[,2]<-if_else(block_temp[,2] < 0, 
                                abs(block_temp[,2]), 
                                block_temp[,2])
      }
      
      ## Optimal search of E dimension on Surrgate series
      out.temp <- do.call(rbind,lapply(1:MaxE, function(E_i){
        tp<-TimeForPred-1
        libsize_max<-libSize_max(N = block_temp, tau = tau, E = E_i, tp = tp)
        pred_ccm <- make_pred_nozero(Target,E_i)
        ccm(block=block_temp,
            E=E_i,
            lib=lib_ccm,
            pred=pred_ccm,
            lib_sizes = libsize_max,
            exclusion_radius=0,
            random_libs = FALSE,
            num_sample=1,
            tp = tp,
            lib_column = 1,
            target_column = 2)
      }))
      
      ## Optimal E for the Surrogate series
      E_star[i] <- out.temp$E[which.max(out.temp$`V1:V2`[2:MaxE])+1]
      pred_ccm <- make_pred_nozero(Target,E_star[i])
      tp<-TimeForPred
      
      libsize_max<-libSize_max(N = block_temp, tau = tau, E = E_star[i], tp = tp)
      
      ## Surrogate CCM with the E_star dimension
      df.out.ccm <- ccm(block=block_temp,E=E_star[i],
                        lib=lib_ccm,
                        pred = pred_ccm,
                        lib_sizes = libsize_max,
                        exclusion_radius=0,
                        random_libs = FALSE,
                        num_sample=1,
                        tp = tp)
      
      ## Keeping the surrogate statistics
      RhoSurr[(i+1),j-1]<-df.out.ccm$`V1:V2`
      RhoSurr[(i+1), 7]<-i
      
      # ## Keeping surrogate series
      # Surr<-cbind(Surr, block_temp[,2])
      
    } # end loop surrogates
    print(paste0("All surrogates done for ", DriversNames[j-1]))
    
    ## Original
    block_temp[,2]<-Original[,j]
    
    # Optimal E for original
    out.temp <- do.call(rbind,lapply(1:MaxE, function(E_i){
      tp<-TimeForPred-1
      libsize_max<-libSize_max(N = block_temp, tau = tau, E = E_i, tp = tp)
      pred_ccm <- make_pred_nozero(Target,E_i)
      ccm(block=block_temp,
          E=E_i,
          lib=lib_ccm,
          pred=pred_ccm,
          lib_sizes = libsize_max,
          exclusion_radius=0,
          random_libs = FALSE,
          num_sample=1,
          tp = tp,
          lib_column = 1,
          target_column = 2)
    }))
    
    ## Optimal E for Original time series
    E_starOriginal<- out.temp$E[which.max(out.temp$`V1:V2`[2:MaxE])+1]
    pred_ccm <- make_pred_nozero(Target,E_starOriginal)
    tp<-TimeForPred
    
    libsize_max<-libSize_max(N = block_temp, tau = tau, E = E_starOriginal, tp = tp)
    df.out.ccm <- ccm(block=block_temp,
                      E=E_starOriginal,
                      lib=lib_ccm,
                      pred = pred_ccm,
                      lib_sizes = libsize_max,
                      exclusion_radius=0,
                      random_libs = FALSE,
                      num_sample=1,
                      tp = tp)
    ## Keeping original statistics, first line of RhoSurr Matrix
    RhoSurr[1,j-1]<-df.out.ccm$`V1:V2`
    RhoSurr[1,7]<-"Original"
    
    ## Messages
    print(paste0("All done for ", DriversNames[j-1]))
    
  } #end loop drivers
  
  # check significance
  for (k in 1:length(Drivers)){
    signf[k]<-(sum(RhoSurr[1,k]>RhoSurr[,k])/NSurr)>(1-alpha)
  }
  
  ## Preparing for saving objects
  RhoSurr<-as.data.frame(RhoSurr,row.names = F)
  RhoSurr$tp<-TimeForPred
  colnames(RhoSurr)<-DriversNames
  
  ## Saving csv
  vroom_write(RhoSurr,
              paste0('Outputs/Tables/rj/Boxplot_tp=',TimeForPred,'.csv.xz'))
  
  # ## Saving Surr
  # Surr<-as.data.frame(Surr) |> 
  #   setNames(str_c(DriversNames))
  # 
  # vroom_write(Surr, 
  #             paste0('Outputs/Tables/rj/surrogates_tp=', TimeForPred, '.csv.xz'))
  
  # ## plots
  # png(filename = paste0("Outputs/Plots/rj/boxplot_tp=", -i), width = 9, height = 7, units = "in")
  # boxplot(RhoSurr)
  # title(main =paste('tp= ',TimeForPred,sep=''),ylab='rho')
  # points(seq(1,length(Drivers)),RhoSurr[1,],pch=16,cex=1.25,lwd=1.25)
  # points(seq(1,length(Drivers))[signf],RhoSurr[1,signf],pch=8,col='red',cex=2,lwd=2)
  # dev.off()
  # 
  # # ## ggplot
  # # rhosurr_longer<-RhoSurr %>% 
  # #   pivot_longer(names_to = "driver", values_to = "rho", cols = everything()) %>% 
  # #   mutate(driver = as.factor(driver))
  # # signf[,2]<-RhoSurr[1,]
  # # 
  # # rhosurr_boxplot<- rhosurr_longer %>% 
  # #   ggplot(aes(x = driver , y = rho))+
  # #   geom_boxplot()
  # # rhosurr_boxplot
  
  return(RhoSurr)
  
} # end function
