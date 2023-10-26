
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Heterogeneous AutoRegressive (HAR) model

<!-- badges: start -->
<!-- [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) -->

[![CRAN
status](https://www.r-pkg.org/badges/version/haR)](https://CRAN.R-project.org/package=haR)
[![devel
version](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/daniGiro/haR)
<!-- badges: end -->

Tools for estimating and forecasting using the Heterogeneous
AutoRegressive (HAR) model by Corsi (2009)
[\<doi:10.1093/jjfinec/nbp001\>](https://doi.org/10.1093/jjfinec/nbp001)
and the Partial-Variances Heterogeneous AutoRegressive model by
Bollerslev et al. (2022)
[\<doi:10.1016/j.jeconom.2021.04.013\>](https://doi.org/10.1016/j.jeconom.2021.04.013).

## Installation

You can install the development version of **haR** from
[GitHub](https://github.com/danigiro/haR) with:

``` r
# install.packages("devtools")
devtools::install_github("daniGiro/haR")
```

## Example

``` r
library(haR)
x <- rnorm(1000)
obj <- har(x)
summary(obj)
#> Call:
#> har(y = x)
#> 
#> Residuals:
#>           Min      1Q   Median     3Q   Max
#> F.H. 1 -2.861 -0.6615 0.009671 0.6948 3.317
#> 
#> Coefficients:
#>         intercept  L1        L5        L22     
#> F.H. 1   0.03158   -0.05467   0.12196  -0.21994
#> 
#> Forecast:
#>       F.H. 1 
#> Mean  0.09406

#coef(obj)
#print(obj)
#fitted(obj)
#residuals(obj)
#predict(obj)
```
