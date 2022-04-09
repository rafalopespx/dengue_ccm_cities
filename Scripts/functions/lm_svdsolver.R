lm_svdsolve <- function(y, x, ws, subset = seq_along(y)){
  x <- x[subset,]
  y <- y[subset]
  ws <- ws[subset]
  # prepended column of 1s for constant term in linear model
  A <- cbind(1, x) * ws
  A_svd <- svd(A)
  # >>REMOVE SMALL SINGULAR VALUES<<
  s <- A_svd$d
  s_inv <- matrix(0, nrow = dim(x)[2]+1, ncol = dim(x)[2]+1)
  for(i in seq_along(s))
  {
    if(s[i] >= max(s) * 1e-5)
      s_inv[i,i] <- 1/s[i]
  }
  coeff <- A_svd$v %*% s_inv %*% t(A_svd$u) %*% (ws * y)
  coeff <- t(coeff)
  colnames(coeff) <- c("const",colnames(x))
  return(coeff)
}