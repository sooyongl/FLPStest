library(tidyverse)

lambda <- matrix(c(1,1,1,0,0,0,0,0,0,1,1,1),ncol = 2)
phi <- matrix(c(1.2, 0.2, 0.2, 1.1), ncol = 2)
error <- diag(1, 6)

mat <- lambda %*% phi %*% t(lambda) + error

data <- data.frame(MASS::mvrnorm(n = 100, rep(0, 6), mat))

data <- list(
  X       = data,
  N       = nrow(data),
  p       = ncol(data)
)

models <- "functions {
  real mvn_lpdf(vector X, vector mu, cov_matrix sigma) {

    real e_part = 0.0;
    real logl = 0.0;

    e_part = (X - mu)' * inverse(sigma) * (X - mu);

    logl = log(2*pi()) + log(determinant(sigma))
    logl = -.5*logl - .5*e_part

    return logl
  }
}

data{
  //Sample sizes
  int<lower=1> N;
  int<lower=1> K;

  matrix[N,K] X;
}

parameters{
  vector[K] eta[N];
  cov_matrix[K] Sigma;
}


model{

	for(i in 1:nstud) {
	  target += mvn_lpdf(eta[i] | rep_vector(0,K), Sigma);
	}

}
// last line blank
"

stan_model(model_code = models)

fit <- rstan::stan(
  # file = "inst/stan/psIRT_multi.stan",
  file = "inst/stan/example_model.stan",
  # file = "inst/stan/psGRM_multi.stan",
  data = sdat$stan_dt,
  # iter = 6000,
  # warmup = 2000,
  chains = 1,
  open_progress = F
)
