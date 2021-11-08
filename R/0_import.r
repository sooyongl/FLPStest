#' @import rstan
#' @import coda
#' @import methods
#' @importFrom utils read.csv packageVersion head tail
#' @importFrom MASS mvrnorm

# library(lavaan)
# library(mirt)
# library(poLCA)
# library(tidyLPA)
# library(flexmix)
# library(broom)
# library(lme4)
# library(mvtnorm)
# library(coda)
# library(rstan)
# library(tidyverse)
# library(ggmcmc)
# library(doSNOW)
# library(doParallel)
# library(foreach)
# library(bayesplot)

NULL

.onAttach <- function(libname, pkgname) {
  if (packageVersion("rstan") < "2.8.0") {
    stop("")
  }
}

.onLoad <- function(...) {
    directoryPath = system.file("", package = "FLPS")
}
