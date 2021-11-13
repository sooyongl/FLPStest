library(FLPS)

# simulation factors ------------------------------------------------------
param_list<-parsFromMod <- list(
  N = 500, # sample size
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
res <- FLPS::runFLPS(
  flps_data = sim_dt,
  lv_model = "rasch",
  stan_options = list(iter = 2000, warmup = 1000, cores = 1, chains = 1))

# res <- FLPS::runSimulation(parsFromMod, iter = 2000, warmup = 1000, cores = 1, chains = 1)
# str(res); names(res)

fit <- as.data.frame(res@flps_fit)
cbind(sim_dt$lv.par,
      -colMeans(fit[stringr::str_detect(names(fit), "secEff\\[")])
)

res_rasch <- res
saveRDS(res_rasch, "results/res_rasch.rds")
rm(res)

# or ------------------------------------------------------------------

for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)

sim_dt <- do.call(FLPS::makeDat, parsFromMod)
stan_model <- loadRstan(lv_model = parsFromMod$lvmodel)

fit <- rstan::stan(
  model_code = stan_model,
  data = sim_dt,
  # iter = 4000,
  # warmup = 1000,
  cores = 1,
  chains = 1
)


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
res <- FLPS::runSimulation(param_list = parsFromMod, iter = 10000, warmup = 2000, cores = 4, chains = 4)
str(res)

fit <- as.data.frame(res@flps_fit)
cbind(sim_dt$lv.par,
colMeans(fit[stringr::str_detect(names(fit), "alpha")]),
colMeans(fit[stringr::str_detect(names(fit), "beta\\[")])
)
res_2pl <- res

saveRDS(res_2pl, "results/res_2pl.rds")
rm(res)
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
res <- FLPS::runSimulation(parsFromMod, iter = 8000, warmup = 2000, cores = 4, chains = 4)
str(res)
res_gpcm <- res

fit <- as.data.frame(res@flps_fit)
cbind(sim_dt$lv.par,
      colMeans(fit[stringr::str_detect(names(fit), "alpha")]),
      colMeans(fit[stringr::str_detect(names(fit), "beta1\\[")]),
      colMeans(fit[stringr::str_detect(names(fit), "beta2\\[")]),
      colMeans(fit[stringr::str_detect(names(fit), "beta3\\[")])
)

saveRDS(res_gpcm, "results/res_gpcm.rds")
rm(res)
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
str(sim_dt)

sim_dt$grad
sim_dt$true_eta
sim_dt$lv.par
res <- FLPS::runSimulation(param_list = parsFromMod, iter = 8000, warmup = 2000, cores = 4, chains = 4)
str(res)
res_sem <- res

fit <- as.data.frame(res@flps_fit)
colMeans(fit[stringr::str_detect(names(fit), "lambda")])
sim_dt$lv.par

saveRDS(res_sem, "results/res_sem.rds")
rm(res)
