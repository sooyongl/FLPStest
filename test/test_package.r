# rm(list = ls())
library(FLPS)
# for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)
# https://mc-stan.org/users/documentation/case-studies/Latent_class_case_study.html#data-generation-and-label-switching

# simulation factors ------------------------------------------------------
param_list <- parsFromMod <- list(
  N = 100,
  R2Y = 0.2,
  omega = 0.2,
  tau0 = 0.13,
  tau1 = -0.06,
  lambda = .6,
  R2eta = 0.2,
  nsec = 20,
  lvmodel = "2pl" # tag for latent variable model
)


# package version ---------------------------------------------------------

## default version
sim_dt <- do.call(makeInpData, parsFromMod);
fit <- runFLPS(
  inp_data = sim_dt$inp_data,
  outcome = "Y",
  group = "Z",
  covariate = c("x1","x2"),
  lv_type = "2pl",
  lv_model = paste0("F =~ ", paste(paste0("X", 1:20), collapse = "+")),
  stan_options = list()
)

## Custom version
custom_dt <- do.call(makeDat, parsFromMod);
custom_stancode <- paste(readLines("inst/stan/psSEM_scaled.stan"), collapse = "\n")

fit <- runFLPS(
  custom_data = custom_dt,
  custom_stan = custom_stancode,
  outcome = "Y",
  group = "Z",
  covariate = c("x1","x2"),
  lv_type = "2pl",
  lv_model = paste0("F =~ ", paste(paste0("X", 1:20), collapse = "+")),
  stan_options = list()
)


# fit <- readRDS("results/res_rasch_new.rds")

fit

fit@inp_data
cat(fit@flps_model)
fit@flps_data
fit@flps_fit

flps_fit <- as.data.frame(fit@flps_fit)
cbind(sim_dt$lv.par,
      est = -colMeans(flps_fit[stringr::str_detect(names(flps_fit), "secEff\\[")])
)

fit_rasch <- fit
saveRDS(fit_rasch, "results/res_rasch_new.rds")
rm(fit)


#######################################################################
#######################################################################
#######################################################################
# or ------------------------------------------------------------------

for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)

sim_dt <- do.call(makeDat, parsFromMod)
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
