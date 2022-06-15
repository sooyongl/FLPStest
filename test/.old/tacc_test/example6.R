library(rstan)
library(tidyverse)
library(foreach)
library(doParallel)
library(Rmpi)

for(i in fs::dir_ls("R", regexp = "r$")) source(i);

# generate data ------------------------------------------
sim_condition <- list(
  N       = 2000, # sample size
  R2Y     = 0.2,
  R2eta   = 0.2,
  omega   = 0.2,  # a1
  tau0    = 0.4,  # b0
  tau1    = -0.2, # b1
  linear  = T,
  lambda  = 1.0,
  nsec    = 20,
  nfac    = 1,
  lvmodel ="gpcm"
)

sdat <- do.call("makeDat", sim_condition)

cl <- getMPIcluster()
registerDoParallel(cl)

o <- foreach(
  i = 1:10,
  .packages = c("rstan"),
  .errorhandling = 'remove'
  # .export = source_funs,
  # .options.snow = opts
) %dopar% {
  fit <- rstan::stan(
    file = "inst/stan/example_model.stan",
    data = sdat$stan_dt
  )

  saveRDS(file.path("results",
                    paste0(sim_condition$lvmodel,"_",i,"rds")))

  NA
}
stopCluster(cl)
