library(FLPS)

# simulation factors ------------------------------------------------------
parsFromMod <- list(
  N = 1000, # sample size
  R2Y = 0.2, ## from app
  omega = 0.2,
  tau0 = 0.13, ## from paper
  tau1 = -0.06, ## from paper "a difference of one IQR in etaT was associated with a reduction of 0.083 in the effect size" 0.083/1.35~~ 0.06
  lambda = 10, ## from data used in model
  R2eta = 0.2, ## from app
  nsec = 20, ## from data used in model
  lvmodel = "rasch" # tag for latent variable model
)

# package version ---------------------------------------------------------
sim_dt <- do.call(FLPS::makeDat, parsFromMod)
res <- FLPS::runSimulation(parsFromMod)
str(res); names(res)

res_rasch <- res
saveRDS(res_rasch, "results/res_rasch.rds")
# or ------------------------------------------------------------------
# rm(list = ls())
# for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)
# sim_dt <- do.call(FLPS::makeDat, parsFromMod)
# stan_model <- loadRstan(lv_model = parsFromMod$lvmodel)
#
# fit <- rstan::stan(
#   model_code = stan_model@model_code,
#   data = sim_dt,
#   # iter = 4000,
#   # warmup = 1000,
#   cores = 1,
#   chains = 1
# )


# 2pl model ------------------------------------------------------
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

# package version ---------------------------------------------------------
sim_dt <- do.call(FLPS::makeDat, parsFromMod)
sim_dt$lv.par
res <- FLPS::runSimulation(parsFromMod)
str(res)
res_2pl <- res

saveRDS(res_2pl, "results/res_2pl.rds")

# gpcm model ------------------------------------------------------
parsFromMod <- list(
  N = 1000, # sample size
  R2Y = 0.2, ## from app
  omega = 0.2,
  tau0 = 0.13, ## from paper
  tau1 = -0.06, ## from paper "a difference of one IQR in etaT was associated with a reduction of 0.083 in the effect size" 0.083/1.35~~ 0.06
  lambda = 10, ## from data used in model
  R2eta = 0.2, ## from app
  nsec = 20, ## from data used in model
  lvmodel = "gpcm" # tag for latent variable model
)

# package version ---------------------------------------------------------
sim_dt <- do.call(FLPS::makeDat, parsFromMod)
sim_dt$lv.par
res <- FLPS::runSimulation(parsFromMod)
str(res)
res_gpcm <- res

saveRDS(res_gpcm, "results/res_gpcm.rds")

# sem model ------------------------------------------------------
parsFromMod <- list(
  N = 1000, # sample size
  R2Y = 0.2, ## from app
  omega = 0.2,
  tau0 = 0.13, ## from paper
  tau1 = -0.06, ## from paper "a difference of one IQR in etaT was associated with a reduction of 0.083 in the effect size" 0.083/1.35~~ 0.06
  lambda = 10, ## from data used in model
  R2eta = 0.2, ## from app
  nsec = 20, ## from data used in model
  lvmodel = "sem" # tag for latent variable model
)

# package version ---------------------------------------------------------
sim_dt <- do.call(FLPS::makeDat, parsFromMod)
sim_dt$lv.par
res <- FLPS::runSimulation(pars = parsFromMod)
str(res)
res_sem <- res

saveRDS(res_sem, "results/res_sem.rds")

