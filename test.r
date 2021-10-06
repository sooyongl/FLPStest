# rm(list = ls())
for(i in fs::dir_ls("R", regexp = "r$")) source(i)

# memory.limit(50000)
# rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())

# data generation ---------------------------------------------------------
parsFromMod <- list(
  N = 1000, # sample size
  R2Y = 0.2, ## from app
  omega = 0.2,
  tau0 = 0.13, ## from paper
  tau1 = -0.06, ## from paper "a difference of one IQR in etaT was associated with a reduction of 0.083 in the effect size" 0.083/1.35~~ 0.06
  lambda = 10, ## from data used in model
  R2eta = 0.5, ## from app
  nsec = 10, ## from data used in model
  lvmodel = "rasch" # tag for latent variable model
)

sdat <- do.call("makeDat", parsFromMod)
# or
# sdat <- makeDat(N = 1000, R2Y = 0.2, omega = 0.2, tau0 = 0.13, tau1 = -0.06, 
#                lambda = 10, R2eta = 0.5, nsec = 10, lvmodel = "rasch")

# test -----------------------------------------------------------------
measMod <- with(sdat, 
                glmer(grad~X[studentM,]+(1|section)+(1|studentM),
                      family=binomial))
 
# method 1
mod0 <- with(sdat,lm(Y[Z==0]~X[Z==0,]))
mod1 <- with(sdat,lm(Y[Z==1]~X[Z==1,]))
# 
## only two covariates so...
mean((coef(mod1)[-1]-coef(mod0)[-1])/fixef(measMod)[-1])

# run stan --------------------------------------------------------------
# system.time(fit <- rstan::stan("R/psRasch.stan",data=sdat))

