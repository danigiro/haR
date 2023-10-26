#' Heterogeneous AutoRegressive (HAR) model
#'
#' Estimation and forecasting using the HAR model by Corsi (2009) \doi{doi:10.1093/jjfinec/nbp001}
#' and the PV-HAR model (Partial-Variances Heterogeneous AutoRegressive model) by Bollerslev et
#' al. (2022) \doi{doi:10.1016/j.jeconom.2021.04.013}.
#'
#' @param y A numeric vector (a realized measure of the integrated volatility).
#' @param X Optionally, a numerical vector or matrix of external regressors,
#' which must have the same number of rows as y.
#' @param lags A numeric vector specifying the lags to be used in the HAR model. Default is \code{c(1, 5, 22)}.
#' @param fh A numeric vector specifying the forecast horizons. Usually, 1 denotes daily volatility
#' (no aggregation), \code{5} weekly volatility, \code{22} monthly volatility. Default is \code{1}.
#' @param log A logical value indicating whether to apply a logarithmic transformation to the data.
#' Default is \code{FALSE}.
#' @param log_correction If \code{TRUE}, mean forecasts and fitted values are produced.
#' @param intercept A logical value indicating whether to include an intercept term in the model.
#' Default is \code{TRUE}.
#' @param min_correction A logical value indicating whether to apply a minimum correction
#' to negative forecast values. Default is \code{FALSE}.
#' @param min_constant A numeric value representing the minimum constant for corrections when
#' \code{min_correction} is applied. Default is \code{0.5}.
#'
#' @return An S3 object of class \link{haR} containing various components with information
#' related to the HAR model and its forecasts.
#'
#' @references
#' Corsi, F., 2009. A simple approximate long-memory model of realized volatility.
#' Journal of Financial Econometrics 7, 174–196. \doi{doi:10.1093/jjfinec/nbp001}.
#'
#' Bollerslev, T., Medeiros, M., Patton, A., Quaedvlieg, R., 2022.
#' From zero to hero: Realized partial (co)variances.
#' Journal of Econometrics 231, 348–360. \doi{doi:10.1093/jjfinec/nbp001}.
#'
#' @examples
#' x <- rnorm(1000)
#' obj <- har(x)
#'
#' @export
har <- function(y, X, lags = c(1,5,22), fh = 1,
                log = FALSE, log_correction = TRUE, intercept = TRUE,
                min_correction = FALSE, min_constant = 0.5){
  func_call <- match.call()
  fh <- sort(fh)
  lags <- sort(lags)
  miny <- min(y[y>sqrt(.Machine$double.eps)], na.rm = TRUE)

  Xlag <- lagXmean(y = c(y, NA), lags = lags)

  lm_data_na <- cbind(Xlag)

  if(!missing(X)){
    X <- rbind(NA, as.matrix(X))
    if(is.null(colnames(X))){
      colnames(X) <- paste0("X", 1:NCOL(X))
    }
    Xtrain <- X[1:NROW(Xlag), , drop=FALSE]
    lm_data_na <- cbind(Xtrain, lm_data_na)
  }

  if(intercept){
    intercept_val <- 1
    lm_data_na <- cbind(intercept = intercept_val, lm_data_na)
  }

  lm_data_na <- cbind(y = c(y, NA), lm_data_na)
  lm_data_na_h <-lm_data_na

  modh <- lapply(fh, function(h){
    yh <- RcppRoll::roll_mean(lm_data_na[,1], n = h, align = "left", fill = NA)
    lm_data_na_h[,1] <- yh

    if(log){
      if(any(lm_data_na_h<sqrt(.Machine$double.eps), na.rm = TRUE)){
        lm_data_na_h[lm_data_na_h<sqrt(.Machine$double.eps)] <- miny*min_constant
      }
      lm_data_na_h <- log(lm_data_na_h)
      lm_data_na_h[, colnames(lm_data_na_h)=="intercept"] <- 1
    }
    mat_na_index <- rowSums(is.na(lm_data_na_h))==0
    lm_data_train <- lm_data_na_h[mat_na_index,,drop = FALSE]
    lm_data_test <- lm_data_na_h[NROW(lm_data_na_h),-1,drop = FALSE]
    mod <- lm.fit(lm_data_train[,-1, drop = FALSE],
                  lm_data_train[,1,drop = TRUE])
    yhat_h <- sum(mod$coefficients * lm_data_test, na.rm = TRUE)

    fitted_val <- rep(NA, length(mat_na_index))
    fitted_val[mat_na_index] <- fitted(mod)
    fitted_val <- fitted_val[1:length(y)]

    res_innovation <- rep(NA, length(mat_na_index))
    res_innovation[mat_na_index] <- residuals(mod)
    res_innovation <- res_innovation[1:length(y)]
    res_response <- res_innovation

    if(log){
      res_response <- yh[1:length(y)] - exp(fitted_val)
      if(log_correction){
        yhat_h <- exp(yhat_h + var(res_response, na.rm = TRUE)/2)
      }else{
        yhat_h <- exp(yhat_h)
      }
    }

    if(min_correction){
      yhat_h[yhat_h<=0] <- miny*min_constant
    }
    return(list(yhat = yhat_h,
                fitted = fitted_val,
                res_response = res_response,
                res_innovation = res_innovation,
                beta = mod$coefficients,
                dfr = mod$df.residual,
                train = lm_data_train,
                Xfuture = lm_data_test))
  })

  beta <- sapply(modh, function(x) x$beta)
  colnames(beta) <- paste0("F.H. ", fh)
  fitted <- sapply(modh, function(x) x$fitted)
  colnames(fitted) <- paste0("F.H. ", fh)
  res_response <- sapply(modh, function(x) x$res_response)
  colnames(res_response) <- paste0("F.H. ", fh)
  res_innovation <- sapply(modh, function(x) x$res_innovation)
  colnames(res_innovation) <- paste0("F.H. ", fh)

  train <- lapply(modh, function(x) x$train)
  names(train) <- paste0("F.H. ", fh)
  Xfuture <- sapply(modh, function(x) x$Xfuture)
  colnames(Xfuture) <- paste0("F.H. ", fh)
  dfr <- sapply(modh, function(x) x$dfr)
  yhat <- sapply(modh, function(x) x$yhat)

  return(structure(list(train = list(coefficients = beta,
                                     fitted = fitted,
                                     dfr = setNames(dfr, fh),
                                     train = train,
                                     residuals = list(innovation = res_innovation,
                                                      response = res_response),
                                     lags = lags,
                                     fh = fh,
                                     log = log,
                                     log_correction = log_correction,
                                     intercept = intercept,
                                     min_correction = min_correction,
                                     min_constant = min_constant),
                        call = func_call,
                        forecasts = list(mean = setNames(yhat, fh),
                                         X = Xfuture)),
                   class = c("haR", "list")))
}
