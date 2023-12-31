% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/haR-class.R
\docType{class}
\name{haR-class}
\alias{haR-class}
\alias{print.haR}
\alias{fitted.haR}
\alias{residuals.haR}
\alias{coef.haR}
\alias{predict.haR}
\alias{summary.haR}
\alias{haR}
\title{S3 Class: haR}
\description{
The "haR" class represents the output object of the Heterogeneous AutoRegressive (HAR)
model created by the \link{har} function. It contains various components and information related
to the HAR model and its forecasts.
}
\section{Slots}{

\describe{
\item{\code{train}}{A list containing various components related to the model training and results,
including coefficients, fitted values, degrees of freedom, training data, and residuals.}

\item{\code{lags}}{A numeric vector representing the lags used in the HAR model.}

\item{\code{fh}}{A numeric vector representing the forecast horizons.}

\item{\code{log}}{A logical value indicating whether a logarithmic transformation was applied to the data.}

\item{\code{log_correction}}{A logical value indicating whether a correction was applied when taking
the logarithm transformation.}

\item{\code{intercept}}{A logical value indicating whether an intercept term was included in the model.}

\item{\code{min_correction}}{A logical value indicating whether a minimum correction was applied to
negative forecast values.}

\item{\code{min_constant}}{A numeric value representing the minimum constant for corrections when
min_correction is applied.}

\item{\code{call}}{A record of the function call, capturing the arguments and settings used when
creating the HAR model.}

\item{\code{forecasts}}{A list containing forecasted values.}
}}

\examples{
x <- rnorm(1000)
obj <- har(x)
summary(obj)
#coef(obj)
#print(obj)
#fitted(obj)
#residuals(obj)
#predict(obj)

}
