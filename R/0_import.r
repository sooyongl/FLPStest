# library(lavaan)
# library(mirt)
# library(poLCA)
library(mvtnorm)
# library(tidyLPA)
# library(flexmix)
# library(broom)
library(coda)
library(rstan)
# library(lme4)
library(tidyverse)
library(ggmcmc)
library(doSNOW)
library(doParallel)
library(foreach)
library("bayesplot")

# readLines("~/.R/Makevars.win")
# 
# example(stan_model,package="rstan",run.dontrun=TRUE)
# 
# remove.packages(c("StanHeaders", "rstan"))
# install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# 
# remove.packages(c("rstantools"))
# install.packages("rstantools", repos = c("Repository for distributing (some) stan-dev R packages | r-packages", getOption("repos")))

# remove.packages("RcppParallel")
# remotes::install_github("hsbadr/RcppParallel@develop", force = TRUE)
# 
# dotR <- file.path(Sys.getenv("HOME"), ".R")
# M <- file.path(dotR, "Makevars.win")
# cat("\n CXX14FLAGS += -O3", file = M, sep = "\n", append = FALSE)
