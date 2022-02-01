rm(list = ls())

# #error Support for C++03 has been removed. The minimum requirement for this library is fully compliant C++11.

# devtools::install_version("rstan", version = "2.17.3", repos = "http://cran.us.r-project.org")
# devtools::install_version("mirt", version = "1.26.3", repos = "http://cran.us.r-project.org")
# devtools::install_version("StanHeaders", version = "2.17.2", repos = "http://cran.us.r-project.org")
# devtools::install_version("Rcpp", version = "0.12.0", repos = "http://cran.us.r-project.org")
# or???? install.packages("Rcpp", repos="https://rcppcore.github.io/drat")
# devtools::install_version("inline", version = "0.3.14", repos = "http://cran.us.r-project.org")
# devtools::install_version("RcppParallel", version = "5.0.1", repos = "http://cran.us.r-project.org")
# devtools::install_version("loo", version = "2.3.0", repos = "http://cran.us.r-project.org")
packageVersion("rstan")
# methods,
# stats4,
# inline,
# gridExtra (>= 2.0.0),
# Rcpp (>= 0.12.0),
# RcppParallel (>= 5.0.1),
# loo (>= 2.3.0),
# pkgbuild,
# V8

library(rstan); # library(stringr): library(dplyr);
for(i in list.files("R",full.names = T)) source(i); rm(i)
source_funs <- ls()



rstan_options(auto_write = TRUE)
options(mc.cores = 8)

file_list <- c(
  "inst/stan/psIRT_multi.stan",
  "inst/stan/psGPCM_multi.stan",
  "inst/stan/psGRM_multi.stan"
)
lvmodel_list <- c("2PL", "GPCM", "GRM")

# Default condition
sim_condition <- list(
  N = 500, # sample size
  R2Y = 0.2,
  omega = 0.2,  # a1
  tau0 = 0.4,  # b0
  tau1 = -0.2, # b1
  lambda = 0.6,
  R2eta = 0.2,
  linear = T,
  nsec = 20,
  nfac = 2,
  lvmodel="2pl")

lvmodel <- sim_condition$lvmodel
nsec    <- sim_condition$nsec
nfac    <- sim_condition$nfac

# setup function ------------------------
setup <- function(sim_condition, i = 1) {
  stan_file_name <- file_list[i]
  lvmodel <- lvmodel_list[i]
  sim_condition$lvmodel <- lvmodel
  
  sim_condition$lvinfo$ipar <- genIRTpar(nitem = nsec, ncat = 3, nfac = nfac, lvmodel = lvmodel)
  
  list(sim_condition = sim_condition,
       stan_file_name = stan_file_name)
}


# set up rstan data ------------------------
setup_data <- setup(sim_condition, 1)
sim_condition  <- setup_data$sim_condition
stan_file_name <- setup_data$stan_file_name

names(sim_condition)
args(makeDat)

sdat <- do.call("makeDat", sim_condition)

# run FLPS ------------------------
names(sdat$stan_dt)
sdat$stan_dt$grad
# fit <- rstan::stan(
#   stan_file_name,
#   data = sdat$stan_dt,
#   iter = 5000,
#   warmup = 2000,
#   chains = 1,
#   open_progress = F
# )


# save --------------------------------------------------------------------
# res <- list(fit = fit, sdat = sdat)
# saveRDS(res, paste0("results/res_irtmulti_tau1.rds"))