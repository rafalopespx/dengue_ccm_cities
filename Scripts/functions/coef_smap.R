

coef_fun<-function(df_N, df, theta, cols, target, max_tp, via = c("smap","block_lnlp"), cutoff){
  
  ## Organizing dataset parsed
  df_N<-df_N %>% 
    select(all_of(target), all_of(cols))
  df<-df %>% 
    select(all_of(target), all_of(cols))
  
  ## Warning for the method used, they give the same output, but SMAP is more flexible
  if(missing(via) | is.null(via) | (!via %in% c("smap", "block_lnlp"))){
    warning("Coefficients estimated via SMap function")
    via<-"smap"
  }
  
  if(via == "smap"){
    
    df_N<-df_N |> 
      mutate(index = row_number()) |> 
      select(index, everything())
    
    lib<-pred<-c(1,nrow(df_N))
    
    coef_series_opt<-SMap(dataFrame = df_N, 
                              E = ncol(df_N), 
                              theta = theta,
                              embedded = T,
                              lib = lib, 
                              pred = pred,
                              columns = cols,
                              target = target)
    
    coef_matrix<-coef_series_opt$coefficients[-1,]
  } else {
    coef_series_opt<-block_lnlp(df_N,
                                theta=theta, ##Optimal theta
                                columns = cols,
                                target_column = target,
                                method = 's-map',
                                tp = 0, 
                                save_smap_coefficients = T)
    
    coef_matrix<-coef_series_opt$smap_coefficients[[1]]
  }
  
  drivers_coef_opt<-data.frame(df, 
                               coef_matrix[,3:length(coef_matrix)])%>% 
    setNames(c("Cases", cols, 
               names(coef_matrix)[3:length(coef_matrix)]))  %>% 
    mutate(date = dengue_t2m_rio$week[(max_tp+1):(cutoff - max_tp)], 
           theta = theta)
  
  return(drivers_coef_opt)
  
}