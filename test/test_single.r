library(rstan)

for(i in list.files("R", full.names = T, pattern = "r$")) source(i);

# generate data ------------------------------------------
sim_condition <- list(
  N       = 1000, # sample size
  R2Y     = 0.3, # 0.1 0.2 0.5
  R2eta   = 0.5, # 0.2 0.5 0.75
  omega   = round(runif(1, 0.1, 0.3),3),
  tau0    = round(runif(1, 0.2, 0.4),3),
  tau1    = round(runif(1, -0.2, -0.1),3),
  linear  = T,
  ydist   = 'n',
  lambda  = 0.6,
  nsec    = 20,
  nfac    = 1,
  lvmodel = 'gpcm'
)
sdat <- do.call("makeDat", sim_condition)

sdat$omega
sdat$tau0
sdat$tau1

# fit_chol <- rstan::stan(
#   file = "inst/stan/psIRT_multi.stan",
#   data = sdat$stan_dt,
#   chain = 4,
#   iter = 6000,
#   warmup = 2000
# )

fit_univ <- rstan::stan(
  file = "inst/stan/psIRT_univ.stan",
  data = sdat$stan_dt,
  chain = 1,
  iter = 2000,
  warmup = 1000,
  cores = 1,
  open_progress = T
)

est_param <- summary(fit_univ,
                     pars = c("betaU","betaU",
                              "betaY","betaY",
                              "b00","a1","b0","b1"))$summary
est_param

eta_param <- summary(fit_univ,
                     pars = c("eta"))$summary
mean(eta_param[,1]); var(eta_param[,1])
mean(sdat$theta); var(sdat$theta)
cor(eta_param[,1], sdat$theta)
plot(eta_param[,1], sdat$theta)

sdat$lv.par[,"a1"]
lam_param <- summary(fit_univ,pars = c("lambda"))$summary

sdat$lv.par[,"d"]
tau_param <- summary(fit_univ,pars = c("tau"))$summary

# o <- list(fit_chol=fit_chol, fit_univ=fit_univ, sdat=sdat)
o <- list(fit=fit_univ, sdat=sdat)

saveRDS(o, "results/test_univ_switched_3.rds")
#
# "results/test_univ_switched_1.rds"
# Warning messages:
# 1: In system(paste(CXX, ARGS), ignore.stdout = TRUE, ignore.stderr = TRUE) :
#   '-E' not found
# 2: There were 4 chains where the estimated Bayesian Fraction of Missing Information was low. See
# http://mc-stan.org/misc/warnings.html#bfmi-low
# 3: Examine the pairs() plot to diagnose sampling problems
#
# 4: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#bulk-ess
# 5: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#tail-ess
#
#
# "results/test_univ_switched_2.rds"
# Warning messages:
#   1: In system(paste(CXX, ARGS), ignore.stdout = TRUE, ignore.stderr = TRUE) :
#   '-E' not found
# 2: There were 4 chains where the estimated Bayesian Fraction of Missing Information was low. See
# http://mc-stan.org/misc/warnings.html#bfmi-low
# 3: Examine the pairs() plot to diagnose sampling problems
#
# 4: The largest R-hat is NA, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#r-hat
# 5: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#bulk-ess
# 6: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#tail-ess


# est_param <- summary(fit_univ,
#                      pars = c("betaU","betaU",
#                               "betaY","betaY",
#                               "b00","a1","b0","b1"))$summary #[, "n_eff"]
#
#
# est_param <- summary(fit_chol,
#                      pars = c("betaU","betaU",
#                               "betaY","betaY",
#                               "b00","a1","b0","b1"))$summary #[, "n_eff"]
