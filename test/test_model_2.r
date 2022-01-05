rm(list = ls())
library(rstan); library(tidyverse)
for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)
source_funs <- ls()

memory.limit(50000)
rstan_options(auto_write = TRUE)
options(mc.cores = 8)

file_list <- c(
  "inst/stan/psIRT_scaled.stan",
  "inst/stan/psGPCM_scaled.stan",
  "inst/stan/psSEM_scaled.stan",
  "inst/stan/psLCA.stan",
  "inst/stan/psLGM.stan",
  "inst/stan/psLPA.stan"
)
lvmodel_list <- c("2PL", "GPCM", "sem", "lgm", "lca","lpa")

# Default condition
sim_condition <- list(
  N = 500, # sample size
  R2Y = 0.2,
  omega = 0.2,  # a1
  tau0 = 0.4,  # b0
  tau1 = -0.2, # b1
  lambda = 0.6,
  R2eta = 0.2,
  nsec = 20,
  linear = F)

# setup function ----------------------------------------------------------
setup <- function(sim_condition, i = 1) {
  stan_file_name <- file_list[i]
  lvmodel <- lvmodel_list[i]
  sim_condition$lvmodel <- lvmodel

  if(i == 1) {
    sim_condition$lvinfo  <-
      list(ipar=data.frame(a = runif(20, 0.7, 1.4), b = rnorm(20), g = rep(0, 20)))
  }
  if(i == 2) {

    sim_condition$lvinfo  <-
      list(ipar=genIRTparam(20, 4, "GPCM", T))
  }
  if(i == 3) {
    sim_condition$lvinfo  <-
      list(cpar=data.frame(loading = round(runif(20, 0.8, 1.5),2)))
  }
  if(i == 4) {
    sim_condition$lvinfo  <-
      list(
        nfac = 2,
        time_loading =
          matrix(
            c(rep(1,20), seq(0, 19, length.out = 20)),
            ncol = 2
          ),
        growth_mean = c(3, 0.1)
      )
  }
  if(i == 5) {
    sim_condition$lvinfo  <- list(separation = 2,nclass = 2)
  }

  list(stan_file_name = stan_file_name,
       sim_condition = sim_condition)
}

# set up rstan data -------------------------------------------------------
setup_dat <- setup(sim_condition, 1)
sdat <- do.call("makeDat", setup_dat$sim_condition)

# run FLPS ----------------------------------------------------------------
fit <- rstan::stan(
  setup_dat$stan_file_name,
  data = sdat,
  iter = 5000,
  warmup = 2000,
  chains = 1,
  open_progress = F
)


# save --------------------------------------------------------------------
res <- list(fit = fit, sdat = sdat)
saveRDS(res, paste0("results/res_", l,"_", sim_condition$lvmodel,i ,".rds"))


