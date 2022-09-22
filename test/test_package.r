# rm(list = ls())
# library(FLPS)
for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)
# https://mc-stan.org/users/documentation/case-studies/Latent_class_case_study.html#data-generation-and-label-switching

# simulation factors ------------------------------------------------------
param_list <- parsFromMod <- list(
  N       = 100, # sample size
  R2Y     = 0.2, # 0.1 0.2 0.5
  R2eta   = 0.5, # 0.2 0.5 0.75
  omega   = round(runif(1, 0.1, 0.3),3),
  tau0    = round(runif(1, 0.2, 0.4),3),
  tau1    = round(runif(1, -0.2, -0.1),3),
  linear  = T,
  ydist   = 'n',
  lambda  = 0.6,
  nsec    = 20,
  nfac    = 1,
  lvmodel = '2pl'
)


# package version ---------------------------------------------------------

## Custom-advanced version
custom_dt <- do.call(makeDat, parsFromMod);
custom_stancode <- paste(readLines("inst/stan/flps_IRT_multi.stan"), collapse = "\n"); # cat(custom_stancode)

# cat(custom_stancode)
# custom_stanmodel <- readRDS("inst/stan/psIRT_scaled.rds")
# custom_stanmodel <- FLPS:::makeStanModel("inst/stan/psIRT_scaled.stan")

fit <- runFLPS(
  custom_data = custom_dt$stan_dt,
  custom_stan = custom_stancode,
  outcome = "Y",
  group = "Z",
  covariate = c("x1","x2"),
  lv_type = "2pl",
  lv_model = paste0("F =~ ", paste(paste0("X", 1:20), collapse = "+")),
  stan_options = list()
)


## user-friendly version
sim_dt <- do.call(FLPS::makeInpData, parsFromMod);
fit <- runFLPS(
  inp_data = sim_dt$inp_data,
  outcome = "Y",
  group = "Z",
  covariate = c("x1","x2"),
  lv_type = "2pl",
  lv_model = paste0("F =~ ", paste(paste0("i", 1:20), collapse = "+")),
  stan_options = list()
)

# mixture -----------------------------------------------------------------
param_list <- parsFromMod <- list(
  N = 100,
  R2Y = 0.2,
  omega = 0.2,
  tau0 = 0.13,
  tau1 = -0.06,
  lambda = .6,
  R2eta = 0.2,
  nsec = 20,
  linear = T,
  lvmodel = "lca", # tag for latent variable model
  lvinfo = 0.9
)
sim_dt <- do.call(makeInpData, parsFromMod)

fit <- runFLPS(
  inp_data = sim_dt$inp_data,
  outcome = "Y",
  group = "Z",
  covariate = c("x1","x2"),
  lv_type = "lca",
  lv_model = paste0("C =~ ", paste(paste0("X", 1:20), collapse = "+")),
  stan_options = list(),
  nclass = 2
)
