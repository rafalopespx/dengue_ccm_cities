theta_find_fun<-function(theta_vec, norm_block, series_block, original, max_tp, length_rj, names_smap){
  
  if(missing(theta_vec)){
    theta_vec<-seq(1,5,.1)
    warning("Theta vector from 1 to 5, by 0.1 steps")
  }
  
  drivers_coef<-vector("list", length(theta_vec))
  coef_matrix<-vector("list", length = length(theta_vec))
  theta_find<-vector("list", length = length(theta_vec))
  
  for (i in 1:length(theta_vec)) {
    coef_series<-block_lnlp(norm_block,
                            theta=theta_vec[i], ## Finding Theta
                            columns = names_smap,
                            target_column = 'cases',
                            method = 's-map',
                            tp = 0, 
                            save_smap_coefficients = T)
    
    coef_matrix[[i]]<-coef_series$smap_coefficients[[1]]
    coef_matrix[[i]]$theta<-theta_vec[i]
    theta_find[[i]]<-c(theta = unique(coef_series$theta), 
                       mae = unique(coef_series$mae), 
                       rmse = unique(coef_series$rmse))
    
    drivers_coef[[i]]<-data.frame(series_block, coef_matrix[[i]][,3:length(coef_matrix[[i]])])%>% 
      setNames(c("Cases", names_smap, 
                 names(coef_matrix[[i]])[3:length(coef_matrix[[i]])]))%>% 
      mutate(date = original$week[(max_tp+1):(length_rj - max_tp)], 
             theta = theta_vec[i])
    
    # names_saved<-paste0('Outputs/Tables/rj/interaction_coef_theta_', i, '.csv.xz')
    # vroom_write(file = names_saved, 
    #             x = drivers_coef)
  }
  
  theta_find<-theta_find %>% 
    bind_rows()
  
  vroom_write(x = theta_find, 
              file = 'Outputs/Tables/rj/yealry_shuffle_theta_mae_rmse.csv.xz')
  
  coef_matrix<-coef_matrix %>% 
    bind_rows()
  
  vroom_write(x = coef_matrix, 
              file = 'Outputs/Tables/rj/yealry_shuffle_coef_matrix.csv.xz')
  
  drivers_coef<-drivers_coef %>% 
    bind_rows()
  
  vroom_write(x = drivers_coef, 
              file = 'Outputs/Tables/rj/yealry_shuffle_drivers_coef.csv.xz')
  
  min_mae<-theta_find$mae[which.min(theta_find$mae)]
  theta_min_mae<-theta_find$theta[which.min(theta_find$mae)]
  
  theta_min_plot<-theta_find %>% 
    ggplot(aes(x = theta, y = mae, col = "MAE"))+
    geom_line()+
    geom_line(aes(x = theta, y = rmse, col = "RMSE"))+
    theme_bw()+
    geom_text(data = theta_find[which.min(theta_find$mae), ], 
              aes(x = theta, 
                  y = mae+.01, 
                  label = theta))+
    geom_text(data = theta_find[which.min(theta_find$rmse), ], 
              aes(x = theta, 
                  y = rmse+.01, 
                  label = theta))+
    labs(x = expression(theta), y = "", 
         title = "Comparison between MAE and RMSE", 
         subtitle = "for the Prediction and Observations")+
    theme(legend.position = "bottom", legend.title = element_blank())
  theta_min_plot
  
  ggsave(filename = 'Outputs/Plots/rj/yearly_shuffle_theta_min_mae_rmse.png', 
         width = 11, 
         height = 9, 
         dpi = 300)
  
  ### Optimal Theta
  theta_opt_mae<-theta_find$theta[which.min(theta_find$mae)]
  
}