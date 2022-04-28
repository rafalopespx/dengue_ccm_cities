#' @param time_series,E
#' @export
#' @examples

make_pred_nozero <- function(time_series,E){
  I_zero_strings <- which(time_series==0)
  I_zero_strings <- Reduce(intersect, lapply((0:E),function(offset) I_zero_strings-offset))
  I_zero_strings <- c(0,I_zero_strings,NROW(time_series)) # for convenience in next step
  N_zero_strings <- length(I_zero_strings)
  lib_nozeroes <- cbind(I_zero_strings[1:(N_zero_strings-1)]+1,I_zero_strings[2:(N_zero_strings)]-1)
  lib_out <- lib_nozeroes[which(lib_nozeroes[,2] > lib_nozeroes[,1]),]
  return(lib_out)
}
