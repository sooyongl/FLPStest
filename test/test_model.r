rm(list = ls())
for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)
source_funs <- ls()

memory.limit(50000)
rstan_options(auto_write = TRUE)

methods(generate)
# dichotomous ---------------------------------------------------------
parsFromMod <- list(
  N = 1000, # sample size
  R2Y = 0.2, ## from app
  omega = 0.2,
  tau0 = 0.13, ## from paper
  tau1 = -0.06, ## from paper "a difference of one IQR in etaT was associated with a reduction of 0.083 in the effect size" 0.083/1.35~~ 0.06
  lambda = 10, ## from data used in model
  R2eta = 0.2, ## from app
  nsec = 20, ## from data used in model
  lvmodel = "2PL" # tag for latent variable model
)

sdat <- do.call("makeDat", parsFromMod)
sdat$lv.par

fit <- stan(
  cores = 1,
  "R/psIRT.stan",
  data = sdat,
  # iter = 4000,
  # warmup = 1000,
  chains = 1
)
str(fit)


# polytomous --------------------------------------------------------------



# sem ---------------------------------------------------------------------
parsFromMod <- list(
  N = 500, # sample size
  R2Y = 0.2, ## from app
  omega = 0.2,
  tau0 = 0.13, ## from paper
  tau1 = -0.06, ## from paper "a difference of one IQR in etaT was associated with a reduction of 0.083 in the effect size" 0.083/1.35~~ 0.06
  lambda = 10, ## from data used in model
  R2eta = 0.2, ## from app
  nsec = 20, ## from data used in model
  lvmodel = "sem" # tag for latent variable model
)

sdat <- do.call("makeDat", parsFromMod)
sdat$lv.par
sdat$grad

fit <- stan(
  cores = 1,
  "R/psSEM.stan",
  data = sdat,
  # iter = 4000,
  # warmup = 1000,
  chains = 1
)
str(fit)

