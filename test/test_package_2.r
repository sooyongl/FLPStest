library(FLPS); # rm(list = ls())
# for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)

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
  linear = T,
  lvmodel = "2pl", # tag for latent variable model
  lvinfo =
    list(ipar=data.frame(a = runif(20, 0.7, 1.4),
                         b = rnorm(20),
                         g = rep(0, 20)))
)


# package version ---------------------------------------------------------
## Custom-advanced version
custom_dt <- do.call(makeDat, parsFromMod);
custom_stancode <- paste(readLines("inst/stan/psIRT_scaled.stan"), collapse = "\n");
# cat(custom_stancode)
# custom_stanmodel <- readRDS("inst/stan/psIRT_scaled.rds")
# custom_stanmodel <- FLPS:::makeStanModel("inst/stan/psIRT_scaled.stan")

fit <- runFLPS(
  custom_data = custom_dt,
  custom_stan = custom_stancode,
  stan_options = list(iter = 5000)
)

print(fit)



# -------------------------------------------------------------------------
# Not yet -------------------------------------------------------------------
# -------------------------------------------------------------------------
## user-friendly version
# sim_dt <- do.call(makeInpData, parsFromMod);
# fit <- runFLPS(
#   inp_data = sim_dt$inp_data,
#   outcome = "Y",
#   group = "Z",
#   covariate = c("x1","x2"),
#   lv_type = "2pl",
#   lv_model = paste0("F =~ ", paste(paste0("X", 1:20), collapse = "+")),
#   stan_options = list()
# )


# mixture -----------------------------------------------------------------
# param_list <- parsFromMod <- list(
#   N = 100,
#   R2Y = 0.2,
#   omega = 0.2,
#   tau0 = 0.13,
#   tau1 = -0.06,
#   lambda = .6,
#   R2eta = 0.2,
#   nsec = 20,
#   linear = T,
#   lvmodel = "lca", # tag for latent variable model
#   lvinfo = 0.9
# )
# sim_dt <- do.call(makeInpData, parsFromMod)
#
# fit <- runFLPS(
#   inp_data = sim_dt$inp_data,
#   outcome = "Y",
#   group = "Z",
#   covariate = c("x1","x2"),
#   lv_type = "lca",
#   lv_model = paste0("C =~ ", paste(paste0("X", 1:20), collapse = "+")),
#   stan_options = list(),
#   nclass = 2
# )
