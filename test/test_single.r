library(rstan)

for(i in list.files("R", full.names = T, pattern = "r$")) source(i);

# generate data ------------------------------------------
sim_condition <- list(
  N       = 2000, # sample size
  R2Y     = 0.2,
  R2eta   = 0.2,
  linear  = T,
  lambda  = 0.6,
  nsec    = 20,
  nfac    = 1,
  ydist   = "n",
  lvmodel ="grm"
)

sdat <- do.call("makeDat", sim_condition)
fit <- rstan::stan(
  file = "inst/stan/psGRM_univ.stan",
  data = sdat$stan_dt,
  chain = 1,
  iter = 1000,
  #warmup = 2000
)

o <- list(fit=fit, sdat=sdat)

saveRDS(o, "results/example_Rasch.rds")
