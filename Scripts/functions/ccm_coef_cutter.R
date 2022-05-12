ccm_coef_cutter<-function(driver_data, original, K, add_col){

## Selecting from the driver data, only significant for the original time series, 
lag_select<-driver_data %>% 
  filter(sig == T)%>% 
  select(tp, rho, driver)%>%
  group_by(driver) %>% 
  arrange(desc(rho)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(tp_abs = abs(tp))

## Amount of driver that are significant
if(missing(add_col)){
  drivers_amount<-length(unique(lag_select$driver))
} else {
  drivers_amount<-length(unique(lag_select$driver))+1L
}

length_rj<-nrow(original)
max_tp<-max(lag_select$tp_abs)-K
size_lagged<-(max_tp+1):(length_rj - max_tp)

series_cut<-series_cut_N<-matrix(NA, 
                                 nrow = length(size_lagged), 
                                 ncol = (drivers_amount+1))

Cases<-original %>% 
  select(Cases) %>% 
  slice((max_tp+1):(length_rj - max_tp))

series_cut[,1]<-Cases$Cases

if(!missing(add_col)){
  added_column<-original |> 
    select(all_of(add_col)) |> 
    slice((max_tp+1):(length_rj - max_tp)) |> 
    setNames(c("added_col"))
  
  series_cut[,(drivers_amount+1)]<-added_column$added_col
}

names_driver<-lag_select$driver
for (i in 1:length(names_driver)) {
  tp_driver<-lag_select$tp[which(lag_select$driver == names_driver[i])]
  
  start_size<-(max_tp - abs(tp_driver))+1
  end_size<-(length_rj-max_tp)-abs(tp_driver)
  
  # if(missing(add_col) | is.null(add_col)){
    driver<-original %>% 
      select(all_of(names_driver[i])) %>% 
      slice((start_size):(end_size))%>% 
      setNames("driver")
    series_cut[,i+1]<-driver$driver
  # } else {
  #   add_col<-names(original)[grep({{add_col}}, names(original))]
  #   
  #   driver<-original %>% 
  #     select(all_of(names_driver[i]), all_of(add_col)) %>% 
  #     slice((start_size):(end_size))%>% 
  #     setNames("driver")
  #   series_cut[,i+1]<-driver$driver
  # }
  
}

names_lag<-str_c(lag_select$driver, lag_select$tp_abs-K)

if(missing(add_col)){
  colnames(series_cut)<-c('cases', names_lag)
}else{
  colnames(series_cut)<-c('cases', names_lag, str_c({{add_col}},0))
}

series_cut<-as.data.frame(series_cut)

block <- series_cut
norm_consts <- apply(block, 2, function(x) sd(x))
block <- as.data.frame(apply(block, 2, function(x) (x-mean(x))/sd(x)))

return(list(Norm_block = block, Series = series_cut, max_tp = max_tp))
}

#