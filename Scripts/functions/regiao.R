regiao<- function(x, english = FALSE){
  x<-as.data.frame(x)
  if(missing(english) | english == FALSE){
    x$regiao <- NA
    x[x$abbrev_state %in% c("RS", "SC", "PR"), "regiao"] <- "Sul"
    x[x$abbrev_state %in% c("AM", "AC", "AP", "PA", "RO", "RR", "TO"), "regiao"] <- "Norte"
    x[x$abbrev_state %in% c("MS", "MT", "GO", "DF"), "regiao"] <- "Centro-Oeste"
    x[x$abbrev_state %in% c("SP", "RJ", "MG", "ES"), "regiao"] <- "Sudeste"
    x[x$abbrev_state %in% c("BA", "PE", "PB", "MA", "CE", "RN", "AL", "SE", "PI"), "regiao"] <- "Nordeste"
  } else {
    x$region <- NA
    x[x$abbrev_state %in% c("RS", "SC", "PR"), "region"] <- "South"
    x[x$abbrev_state %in% c("AM", "AC", "AP", "PA", "RO", "RR", "TO"), "region"] <- "North"
    x[x$abbrev_state %in% c("MS", "MT", "GO", "DF"), "region"] <- "Center-West"
    x[x$abbrev_state %in% c("SP", "RJ", "MG", "ES"), "region"] <- "Southeast"
    x[x$abbrev_state %in% c("BA", "PE", "PB", "MA", "CE", "RN", "AL", "SE", "PI"), "region"] <- "Northeast"
  }
  return(x)
}