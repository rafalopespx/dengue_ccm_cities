
#' Extracts the temporal aggregation seasonality component of the given time series by adjusting
#' a smooth.spline function the time series
#' Author: Rafael Lopes Paixao da Silva
#'
#' @param DataFile A path to the .csv file which seasonality should be extracted
#' @param DateCol The index of the Dates column
#' @param Index_var The indexes of climate variable to which extract seasonality
#' @param DateFormat [Depracted] Date format to be used
#' @param silent To make warnings silented [Default] is FALSE
#' @param plot [logical] Should be plots be done? [Deafult] is FALSE
#' @param save.plot [logical] Should be plots be saved? [Default] is FALSE
#' @param save [logical] Should be the data.frame be saved as .csv? [Default] is TRUE
#' @param complete To return the entry dataset with the seasonality extract? [Default] is FALSE
#'
#' @return
#' @export
#'
#' @examples
SeasonalSplines<-function(DataFile='../data/DataMalariaTartagalCCM.csv',
                          DateCol=1,
                          Index_var,
                          DateFormat='%Y-%m-%d', 
                          silent = F, 
                          plot = F,
                          save.plot = F,
                          save = T,
                          complete = F){
  require(vroom)
  require(lubridate)
  require(tidyverse)
  require(viridis)
  
  if(!silent){
    if(missing(DateCol)){
      DateCol<-1
      warning("Using DateCol as the first column")
    } else {
      DateCol<-DateCol
    }
    
    if(missing(Index_var)){
      Index_var<-ncol(DataSeries[,-DateCol])
      warning("Using as Index_var the rest of the data.frame")
    } else {
      Index_var<-Index_var
    }
  }
  
  DataSeries<-vroom(DataFile) %>% 
    rename(date = week) %>% 
    mutate(doy = yday(date))
  OriginalDate<-DataSeries$date
  VariablesNames<-names(DataSeries)
  
  AgDados<-aggregate(DataSeries[,-DateCol], 
                     by=list(DataSeries$doy), 
                     FUN=mean, 
                     na.rm=TRUE)
  
  SeasonalData<-as.data.frame(matrix(NA,
                                     nrow = nrow(AgDados),
                                     ncol = (length(Index_var)+1))
  ) %>% 
    rename(doy = V1) %>% 
    mutate(doy = row_number())
  names(SeasonalData)<-c("doy", names(DataSeries[,-DateCol])[Index_var])
  WDados<- table(DataSeries$doy)
  Variables<-names(DataSeries[-DateCol])[Index_var]
  
  
  for (i in 2:(length(Variables)+1)){
    AgDadosAux<-rep(AgDados[,i],3)
    SeasonalDataAux<-smooth.spline(AgDadosAux,
                                   w = rep_len(WDados,
                                               length.out = length(AgDadosAux)))$y
    
    SeasonalData[,i]<-SeasonalDataAux[367:(367+nrow(SeasonalData)-1)]
  }
  
  DataPlot<-DataSeries %>% 
    left_join(SeasonalData, by = "doy", suffix = c("","_seasonality"))
  
  if(plot){
    plot_seasonality<-vector("list", (length(Variables)+1))
    for (i in 2:(length(Variables)+1)) {
      vars<-DataPlot[grep(Variables[i-1], names(DataPlot))]
      time_vars<-DataPlot$date
      
      plot_df<-as.data.frame(cbind(time_vars, vars)) %>% 
        setNames(c("date", "var", "seasonality")) %>% 
        mutate(year = factor(year(date)), 
               doy = yday(date))
      
      
      plot_seasonality[[i-1]]<-plot_df %>%
        ggplot(aes(x = doy,
                   y = var, col = year))+
        geom_line()+
        geom_line(aes(x = doy,
                      y = seasonality,
                      col = "Seasonality"))+
        theme_bw()+
        labs(x = "day of the year",
             y = paste0(VariablesNames[i]))+
        scale_color_manual(name = "year", 
                           values = c(viridis(n= 11, 
                                              option = "cividis", 
                                              begin = .3, 
                                              direction = -1), "firebrick1"))
      
      if(save.plot){
        ggsave(filename = paste0("Outputs/Plots/rj/seasonality_", Variables[i-1], ".png"), 
               plot = plot_seasonality[[i-1]], 
               width = 9, 
               height = 7, 
               dpi = 300)
      }
    }
  }
  
  if(save){
    if(complete){
      vroom_write(DataPlot,
                  paste0('Outputs/Tables/rj/Seasonality_complete_',basename(DataFile)))
    }
    vroom_write(SeasonalData,
                paste0('Outputs/Tables/rj/Seasonality_',basename(DataFile)))
  }
  
  
  if(complete){
    if(plot){
      return(list(plots = plot_seasonality, SeasonalData = DataPlot))
    }
    return(DataPlot)
  }
  
  if(plot){
    return(list(plots = plot_seasonality, SeasonalData = SeasonalData))
  }
  return(SeasonalData)
}
