count_model<-function(ncases){
  require(zoo)
  require(tidyverse)
  require(rjags)
  require(R2jags)
  require(R2WinBUGS)
  require(INLA)
  
  ## ----data reading--------------------------------------------------------
  ncases <- data[,{{ 'Cases' }}]
  
  # ----Poisson model, eval=FALSE-------------------------------------------
  
  ## N de sites e ocasioes
  totcasos <- ncases$Cases
  nIntervals <- length(totcasos)
  
  ## Lista com dados
  data.list <- list(nIntervals=nIntervals, y=totcasos)
  ## Valores iniciais baseados nos casos observados:
  ## Tive que usar valores fixos e baseados nos dados pq alternativas (comentadas)
  ## dao o famoso erro "Node inconsistent with parents"
  # inits <- function() list(
  #                         N = rpois(length(totcasos), totcasos*1/0.8+0.01),
  #                         r = rnorm((nIntervals-1), log((totcasos[-(length(totcasos))]+0.01)/(totcasos[-1]+0.01)),0.1),
  #                         lp = rnorm(2,0.1)
  #                     )
  
  inits <- function() list(
    N = ceiling(totcasos*1/0.5+0.01),
    r = log((totcasos[-(length(totcasos))]+0.01)/(totcasos[-1]+0.01)),
    lp = log(0.1/0.9)
  )
  ## Parametros a acompanhar
  pars <- c("N", "mu", "ypred", "p", "r")
  ## Parallel
  fit1 <- jags.parallel(data = data.list,
                        inits= inits,
                        parameters.to.save = pars,
                        model.file = "~/Desktop/malariaCCM/count_models/poisson_model.jag",
                        n.iter = 1e4,
                        # n.burnin = 1e5,
                        n.chains=4,
                        n.cluster = 4,
                        export=c("totcasos", "nIntervals"))
  # save.image()
  
  ## ----plots---------------------------------------------------------------
  ## Predicted x observed number of cases
  tmp <- fit1$BUGSoutput$summary
  totais <- zoo(
    data.frame(obs=ncases$cases, 
               Ncasos=tmp[grep("ypred",rownames(tmp)),"mean"],
               Ncasos.low=tmp[grep("ypred",rownames(tmp)),"2.5%"],
               Ncasos.up=tmp[grep("ypred",rownames(tmp)),"97.5%"]),
    order.by=as.Date(ncases$date))
  
  ## N de casos obs e previsto
  totais.df <- totais %>%
    fortify() %>%
    mutate(data=as.Date(Index))
  
  totaisW.df %>%
    ggplot(aes(data, obs)) +
    geom_line(colour="red") +
    geom_line(aes(data, Ncasos), colour="blue")+
    geom_ribbon(aes(ymin=Ncasos.low, ymax=Ncasos.up), fill="blue", alpha=0.5) +
    scale_y_continuous(name="N cases") +
    theme_bw()
  
  return(totais.df)
}