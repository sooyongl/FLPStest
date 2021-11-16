# rm(list= ls())
library(tidyverse); library(rstan)
source("test/test_scaling/make_data.r"); source("test/test_scaling/lvmodeldata.r")

rstan_options(auto_write = TRUE)

param_list <- parsFromMod <- list(
  N = 500, 
  R2Y = 0.2,
  omega = 0.2,
  tau0 = 0.13,
  tau1 = -0.06,
  lambda = 10,
  R2eta = 0,
  nsec = 10,
  lvmodel = "2pl" # tag for latent variable model
)

sim_dt <- do.call(makeDat, parsFromMod)

mean(sim_dt$true_eta); var(sim_dt$true_eta)

sim_dt$lv.par
sim_dt$original_data

# Original syntax ----------------------------------------------------
stan_model <- paste(read_lines("test/test_scaling/stan/IRT.stan"), collapse = "\n")
cat(stan_model)
original.fit <- rstan::stan(
  model_code = stan_model,
  data = sim_dt,
  iter = 4000,
  warmup = 1000,
  cores = 4,
  chains = 4
)

# compare estimates with population values
sim_dt$lv.par
print(
  original.fit,
  digits = 3,
  par    = c('disc', 'diff'),
  probs  = c(.025, .5, 0.975)
)

# vectorized syntax ----------------------------------------------------
stan_model <- paste(read_lines("test/test_scaling/stan/IRT_2.stan"), collapse = "\n")
cat(stan_model)
sdata <- list(
  N = nrow(sim_dt$original_data),
  J = ncol(sim_dt$original_data),
  grad = sim_dt$original_data
)

vetorized.fit <- stan(
  model_code = stan_model,
  data = sdata,
  thin = 4,
  iter = 4000,
  warmup = 1000,
  cores = 4
)

sim_dt$lv.par
print(
  vetorized.fit,
  digits = 3,
  par    = c('discrim', 'difficulty'),
  probs  = c(.025, .5, 0.975)
)

# save ---------------
saveRDS(list(sim_dt = sim_dt, original.fit = original.fit, vetorized.fit = vetorized.fit),
        "test/test_scaling")