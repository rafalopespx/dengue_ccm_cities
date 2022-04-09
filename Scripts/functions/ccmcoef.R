ccmcoef<-function(df, original_df, length_original){
  
  lags_drivers<-df %>% 
    filter(sig == T) %>% 
    group_by(driver) %>% 
    filter(rho == max(rho)) %>% 
    select(tp, driver) %>% 
    mutate(tp = abs(tp))
  
  drivers_names<-unique(lags_drivers$driver)
  drivers_amount<-length(drivers_names)
  
  if(missing(length_original)){
    length_original<-nrow(original_df)
  }
  length_original<-length_original
  size_lagged_mat<-length_original - max(lags_drivers$tp)
  start_lagged_mat<-length_original-size_lagged_mat
  
  series_mat<-matrix(NA, 
                     nrow = size_lagged_mat, 
                     ncol = (drivers_amount+1))
  
  series_mat[,1]<-original_df$Cases[(1+max(lags_drivers$tp)):length_original]
  
  for (i in drives) {
    series_cut[,2]<-dengue_t2m_rio$temp_min[1:(length_rj - lag_select$tp[lag_select$driver == "temp_min"])]
    series_cut[,3]<-dengue_t2m_rio$total_precip_min[2:(length_rj - lag_select$tp[lag_select$driver == "total_precip_min"])]
    series_cut[,4]<-dengue_t2m_rio$temp_mean[(1+max(lag_select$tp)):length_rj]
  }
  
  series_cut_N<-matrix(NA,
                       nrow = size_lagged_rj,
                       ncol = (drivers_amount+1))
  for (i in 1:ncol(series_cut)){
    series_cut_N[,i]<-(series_cut[,i]-mean(series_cut[,i],na.rm=T))/sd(series_cut[,i],na.rm=T)
  }
  
  colnames(series_cut_N)<-colnames(series_cut)<-c('cases',
                                                  paste0('tmin2'),
                                                  paste0('precip_min1'),
                                                  paste0('tmean0'))
  series_cut_N<-as.data.frame(series_cut_N)
  
  coef_series<-block_lnlp(series_cut_N,
                          theta=1.7,
                          columns = c('tmin2','precip_min1','tmean0'),
                          target_column = 'cases',
                          method = 's-map',
                          tp = 0,
                          save_smap_coefficients = T)
  
  coef_matrix<-coef_series$smap_coefficients[[1]]
  
  drivers_coef<-data.frame(series_cut, coef_matrix[,3:length(coef_matrix)]) %>% 
    setNames(c("Cases", "temp_min2", "precip_min1", "temp_mean0", 
               names(coef_matrix)[3:length(coef_matrix)])) %>% 
    mutate(index = row_number())
}
