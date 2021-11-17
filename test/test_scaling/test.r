# rm(list= ls()); gc()
library(tidyverse); library(rstan);library(ltm)
source("test/test_scaling/make_data.r"); source("test/test_scaling/lvmodeldata.r")

rstan_options(auto_write = TRUE)

param_list <- parsFromMod <- list(
  N = 500,
  R2Y = 0.2,
  omega = 0.2,
  tau0 = 0.13,
  tau1 = -0.06,
  lambda = 10,
  R2eta = 0,
  nsec = 10,
  lvmodel = "2pl" # tag for latent variable model
)

sim_dt_2pl <- do.call(makeDat, parsFromMod)

mean(sim_dt_2pl$true_eta); var(sim_dt_2pl$true_eta)

sim_dt_2pl$lv.par
sim_dt_2pl$original_data

# check IRT model --------------------------------------------------
irt_2pm_par2 = ltm::ltm(sim_dt_2pl$original_data ~ z1, IRT.param = TRUE)

# Original syntax ----------------------------------------------------
stan_model <- paste(read_lines("test/test_scaling/stan/IRT_4.stan"), collapse = "\n")
cat(stan_model)
twopl.fit <- rstan::stan(
  model_code = stan_model,
  data = sim_dt_2pl,
  iter = 4000,
  cores = 1,
  chains = 3
)

# compare estimates with population values
twopl.fit <- as.data.frame(twopl.fit)
fit <- twopl.fit
est_disc <- colMeans(fit[str_detect(names(fit), pattern = "disc1")])
est_diff0 <- colMeans(fit[str_detect(names(fit), "diff1")])
est_diff1 <- colMeans(fit[str_detect(names(fit), "difficulty")])

sim_dt <- sim_dt_2pl

cbind(pop_disc = sim_dt$lv.par[,1],
      est_disc = est_disc,
      pop_dff = sim_dt$lv.par[,2],
      est_diff = est_diff1,
      est_int = est_diff0)

eta <- cbind(est_eta = colMeans(fit[str_detect(names(fit), "eta")]), pop_eta = sim_dt$true_eta)

colMeans(eta ); apply(eta, 2, var)
plot(eta[,"est_eta"],eta[,"pop_eta"])


# CFA ---------------------------------
param_list <- parsFromMod <- list(
  N = 500,
  R2Y = 0.2,
  omega = 0.2,
  tau0 = 0.13,
  tau1 = -0.06,
  lambda = 6,
  R2eta = 0,
  nsec = 10,
  lvmodel = "sem"
)

sim_dt_cfa <- do.call(makeDat, parsFromMod)

sim_dt_cfa$original_data
sim_dt_cfa$lv.par

stan_model <- paste(read_lines("test/test_scaling/stan/CFA.stan"), collapse = "\n")
cat(stan_model)
cfa.fit <- rstan::stan(
  model_code = stan_model,
  data = sim_dt_cfa,
  iter = 4000,
  cores = 1,
  chains = 3
)

sim_dt <- sim_dt_2pl

cfa.fit <- as.data.frame(cfa.fit)

saveRDS(
  list(
    sim_dt_cfa = sim_dt_cfa,
    cfa.fit = cfa.fit),
  "test/test_scaling/cfa_result.rds")

fit <- cfa.fit
est <- colMeans(fit[str_detect(names(fit), "lambda1")])
est0 <- colMeans(fit[str_detect(names(fit), "lambda")])
cbind(pop = sim_dt$lv.par, est = est, est_noncont = est0)


eta <- cbind(est_eta = colMeans(fit[str_detect(names(fit), "eta")]), pop_eta = sim_dt$true_eta)

colMeans(eta ); apply(eta, 2, var)
plot(eta[,"est_eta"],eta[,"pop_eta"])


# save ---------------
saveRDS(
  list(
    sim_dt_irt = sim_dt_irt,
    twopl.fit = twopl.fit,
    sim_dt_cfa = sim_dt_cfa,
    cfa.fit = cfa.fit),
  "test/test_scaling/result.rds")

