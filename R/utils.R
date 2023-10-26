lagXmean <- function(y, lags){
  lagX <- lag_mat(y, lags = c(1:max(lags)))
  out <- sapply(lags, function(l){
    rowMeans(lagX[, 1:l, drop = FALSE])
  })
  colnames(out) <- paste0("L", lags)
  out
}

lag_mat <- function(x, lags) {
  lmat <- sapply(lags, function(i){
    if(i > 0){
      c(rep(NA, i), x[1:(length(x) - i)])[1:length(x)]
    }else if (i < 0){
      c(x[(abs(i) + 1):length(x)], rep(NA, abs(i)))
    }else{
      x
    }
  })
  return(lmat)
}
