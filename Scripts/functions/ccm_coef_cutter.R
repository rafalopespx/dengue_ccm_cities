ccm_coef_cutter<-function(driver_surr, original, K){


lag_select<-driver_surr %>% 
  filter(sig == T)%>% 
  # select(tp, rho)%>% 
  group_by(driver) %>% 
  arrange(desc(rho)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(tp_abs = abs(tp))

drivers_amount<-length(unique(lag_select$driver))

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

names_driver<-lag_select$driver
for (i in 1:length(names_driver)) {
  tp_driver<-lag_select$tp[which(lag_select$driver == names_driver[i])]
  
  start_size<-(max_tp - abs(driver_data$tp))+1
  end_size<-(length_rj-max_tp)-abs(driver_data$tp)
  driver<-original %>% 
    select(all_of(names_driver[i])) %>% 
    slice((start_size):(end_size)) %>% 
    setNames("driver")
  series_cut[,i+1]<-driver$driver
  
}

names_lag<-str_c(lag_select$driver, lag_select$tp_abs-K)

colnames(series_cut)<-c('cases', names_lag)
series_cut<-as.data.frame(series_cut)

block <- series_cut
norm_consts <- apply(block, 2, function(x) sd(x))
block <- as.data.frame(apply(block, 2, function(x) (x-mean(x))/sd(x)))

return(list(Norm_block = block, Series = series_cut))
}