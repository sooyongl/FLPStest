library(rstan); library(tidyverse)
for(i in fs::dir_ls("R", regexp = "r$")) source(i);

# generate data -----------------------------------------------------------
sim_condition <- list(
  N = 2000, # sample size
  R2Y = 0.2,
  omega = 0.2,  # a1
  tau0 = 0.4,  # b0
  tau1 = -0.2, # b1
  lambda = 0.6,
  R2eta = 0.2,
  linear = T,
  nsec = 20,
  nfac = 2,
  lvmodel="2pl",
  lvinfo =
    list(
      ipar = round(genIRTpar(nitem = 20,
                             nfac = 2,
                             lvmodel = "2pl"),3)
    )
)

sdat <- do.call("makeDat", sim_condition)
stan_dt <- sdat$stan_dt

# item parameter
sdat$lvinfo$ipar

# factor index
stan_dt$factoridx
stan_dt$firstitem


# run flps ----------------------------------------------------------------
fit <- rstan::stan(
  file = "inst/stan/psIRT_multi.stan",
  data = stan_dt,
  iter = 10000,
  warmup = 2000,
  chains = 1,
  open_progress = F
)

# cleaning ----------------------------------------------------------------
res <- clean_temp(fit, sdat)
saveRDS(res, "test/test_model_0.rds")

