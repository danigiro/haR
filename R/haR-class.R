#' S3 Class: haR
#'
#' The "haR" class represents the output object of the Heterogeneous AutoRegressive (HAR)
#' model created by the \link{har} function. It contains various components and information related
#' to the HAR model and its forecasts.
#'
#' @aliases print.haR fitted.haR residuals.haR coef.haR predict.haR summary.haR
#'
#' @slot train A list containing various components related to the model training and results,
#' including coefficients, fitted values, degrees of freedom, training data, and residuals.
#' @slot lags A numeric vector representing the lags used in the HAR model.
#' @slot fh A numeric vector representing the forecast horizons.
#' @slot log A logical value indicating whether a logarithmic transformation was applied to the data.
#' @slot log_correction A logical value indicating whether a correction was applied when taking
#' the logarithm transformation.
#' @slot intercept A logical value indicating whether an intercept term was included in the model.
#' @slot min_correction A logical value indicating whether a minimum correction was applied to
#' negative forecast values.
#' @slot min_constant A numeric value representing the minimum constant for corrections when
#' min_correction is applied.
#' @slot call A record of the function call, capturing the arguments and settings used when
#' creating the HAR model.
#' @slot forecasts A list containing forecasted values.
#'
#' @aliases haR
#'
#' @examples
#' x <- rnorm(1000)
#' obj <- har(x)
#' summary(obj)
#' #coef(obj)
#' #print(obj)
#' #fitted(obj)
#' #residuals(obj)
#' #predict(obj)
#'
#' @import methods
#' @export
setClass("haR",
         slots = list(
           train = "list",
           lags = "numeric",
           fh = "numeric",
           log = "logical",
           log_correction = "logical",
           intercept = "logical",
           min_correction = "logical",
           min_constant = "numeric",
           call = "call",
           forecasts = "list"
         )
)


#' @export
print.haR <- function(x,  digits = getOption("digits"), ...){
  cat("Forecast:\n")
  fore <- rbind(x$forecasts$mean)
  rownames(fore) <- "Mean"
  colnames(fore) <- paste0("F.H. ", x$train$fh)
  print.default(format(fore, digits = digits), print.gap = 2L,
                quote = FALSE)
  cat("\n")
}

#' @export
fitted.haR <- function(object, type = c("original", "train"), ...){
  type <- match.arg(type)
  if(type == "original" & object$train$log){
    exp(object$train$fitted)
  }else{
    object$train$fitted
  }
}

#' @export
residuals.haR <- function(object, type = c("innovation", "response"), ...){
  type <- match.arg(type)
  object$train$residuals[[type]]
}

#' @export
coef.haR <- function(object, ...){
  object$train$coefficients
}

#' @export
predict.haR <- function(object, ...){
  object$forecasts$mean
}

#' @export
summary.haR <- function(object, digits = max(3L, getOption("digits") - 3L), ...){
  cat("Call:\n", paste(deparse(object$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")

  resid <- residuals(object, type = "response")
  df <- NROW(resid)
  cat("Residuals:\n", sep = "")
  if (df > 5L) {
    nam <- c("Min", "1Q", "Median", "3Q", "Max")
    if(length(dim(resid)) == 2L){
      rq <- structure(apply(t(resid), 1L, quantile, na.rm = TRUE),
                      dimnames = list(nam, paste0("F.H. ", object$train$fh)))
    }else{
      zz <- zapsmall(quantile(resid, na.rm = TRUE), digits + 1L)
      rq <- structure(zz, names = nam)
    }
    if(length(object$train$fh) == 1){
      print(t(rq), digits = digits, ...)
    }else{
      print(rq, digits = digits, ...)
    }
  }
  cat("\n")
  coef <- object$train$coefficients
  cat("Coefficients:\n")
  if(length(object$train$fh) == 1){
    print.default(format(t(coef), digits = digits), print.gap = 2L,
                  quote = FALSE)
  }else{
    print.default(format(coef, digits = digits), print.gap = 2L,
                  quote = FALSE)
  }
  cat("\n")
  cat("Forecast:\n")
  fore <- rbind(object$forecasts$mean)
  rownames(fore) <- "Mean"
  colnames(fore) <- paste0("F.H. ", object$train$fh)

  print.default(format(fore, digits = digits), print.gap = 2L,
                quote = FALSE)
}
