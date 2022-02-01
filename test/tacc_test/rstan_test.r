# The parallel version (for Stampede2)
library(foreach)
library(doParallel)
library(Rmpi)
library(rstan)

cl <- getMPIcluster()
registerDoParallel(cl)

predictors <- mtcars[ , -1]

stan_data <- list(
  N = 32,
  K = 10,
  X = predictors,
  y = mtcars$mpg
)

fit_rstan <- stan(
  file = "mtcars.stan",
  data = stan_data
)


