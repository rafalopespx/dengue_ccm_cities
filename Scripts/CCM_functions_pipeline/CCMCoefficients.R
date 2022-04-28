# This function obtains the coefficients (interaction strenghts) for each 
# pre-defined causal variable with respective lag.
# The series to be analyzed are manually cut and named in the beginning, the rest just needs to use the same column names.
CCMCoefficients <- function(DataFile='../data/DataMalariaTartagalCCM.csv') {
  series<-read.csv(DataFile)
  series_cut<-matrix(NA,602,6)
  series_cut[,1]<-series$cases[23:624]
  series_cut[,2]<-series$tmin[23:624]
  series_cut[,3]<-series$tmin[1:602]
  series_cut[,4]<-series$tmax[18:619]
  series_cut[,5]<-series$hmin[10:611]
  series_cut[,6]<-series$hmax[10:611]
  series_cut_N<-matrix(NA,602,6)
  for (i in 1:ncol(series_cut)){
    series_cut_N[,i]<-(series_cut[,i]-mean(series_cut[,i],na.rm=T))/sd(series_cut[,i],na.rm=T)
  }
  colnames(series_cut_N)<-c('cases','tmin0','tmin22','tmax','hmin','hmax')
  series_cut_N<-as.data.frame(series_cut_N)
  colnames(series_cut)<-c('cases','tmin0','tmin22','tmax','hmin','hmax')
  series_cut<-as.data.frame(series_cut)
  aux2<-block_lnlp(series_cut_N,theta=1.7,
                   columns = c('tmin0','tmin22','tmax','hmin','hmax'),
                   target_column = 'cases',
                   method = 's-map',
                   tp = 0,
                   save_smap_coefficients = T)
  c_tmin0<-aux2$smap_coefficients[[1]]$c_1
  c_tmin22<-aux2$smap_coefficients[[1]]$c_2
  c_tmax<-aux2$smap_coefficients[[1]]$c_3
  c_hmin<-aux2$smap_coefficients[[1]]$c_4
  c_hmax<-aux2$smap_coefficients[[1]]$c_5
  #tmin0
  plot(series_cut$tmin0,
       c_tmin0,
       xlab='tmin',
       ylab='interaction strength [ tmin lag 0 ]')
  abline(0,0,lty=2,lwd=2)
  #tmin22
  plot(series_cut$tmin22,
       c_tmin22,
       xlab='tmin',
       ylab='interaction strength [ tmin lag 22 ]')
  abline(0,0,lty=2,lwd=2)
  #tmax
  plot(series_cut$tmax,
       c_tmax,
       xlab='tmax',
       ylab='interaction strength [ tmax ]')
  abline(0,0,lty=2,lwd=2)
  #hmax
  plot(series_cut$hmax,
       c_hmax,xlab='hmax',
       ylab='interaction strength [ hmax ]')
  abline(0,0,lty=2,lwd=2)
  #hmin
  plot(series_cut$hmin,
       c_hmin,xlab='hmin',
       ylab='interaction strength [ hmin ]')
  abline(0,0,lty=2,lwd=2)
}
