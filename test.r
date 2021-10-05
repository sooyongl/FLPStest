# rm(list = ls())
for(i in fs::dir_ls("R", regexp = "r$")) source(i)

memory.limit(50000)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

parsFromMod <- list(
  N = 1000, # 5308
  R2Y = 0.2, ## from app
  omega = 0.2,
  tau0 = 0.13, ## from paper
  tau1 = -0.06, ## from paper "a difference of one IQR in etaT was associated with a reduction of 0.083 in the effect size" 0.083/1.35~~ 0.06
  lambda = 10, ## from data used in model
  R2eta = 0.5, ## from app
  nsec = 10, ## from data used in model
  lvmodel = "rasch" # tag for latent variable model
)

sdat <- do.call("makeDat", parsFromMod)
# system.time(sdat <- do.call("makeDat", parsFromMod))

### NOT RUN
system.time(fit <- rstan::stan("R/psRasch.stan",data=sdat))

