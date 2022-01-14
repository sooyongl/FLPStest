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
  "inst/stan/psLGM.stan",
  "inst/stan/psLCA.stan",
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
  linear = T)

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
setup_dat <- setup(sim_condition, 4)
sdat <- do.call("makeDat", setup_dat$sim_condition)

# run FLPS ----------------------------------------------------------------
names(sdat$stan_dt)
sdat$stan_dt$Y
fit <- rstan::stan(
  setup_dat$stan_file_name,
  data = sdat$stan_dt,
  iter = 5000,
  warmup = 2000,
  chains = 1,
  open_progress = F
)


# save --------------------------------------------------------------------
res <- list(fit = fit, sdat = sdat)
saveRDS(res, paste0("results/res_lgm.rds"))


# cleaning ----------------------------------------------------------------
res_list <- fs::dir_ls("results")
res <- readRDS(res_list[1])

fit <- res$fit
sdat <- res$sdat
stan_dt <- res$sdat$stan_dt

iter = fit@sim$iter - fit@sim$warmup
chain = fit@sim$chains

# item param
df.fit <- as.data.frame(fit)
df.fit$chain <- rep(c(1:chain), each = iter)
df.fit <- df.fit %>% group_by(chain)
#
df.fit %>%
  select(matches("^nu|^p|^b00|^b01|betaY|betaU")) %>%
  summarise_all(mean)

lambda <- df.fit %>%
  select(chain, matches("^lambda_free\\[")) %>%
  summarise_all(mean)

tau <- df.fit %>%
  select(chain, matches("^tau\\[")) %>%
  summarise_all(mean)


# lambda <- colMeans(df.fit[str_detect(names(df.fit), "alpha\\[")])
# tau <- colMeans(df.fit[str_detect(names(df.fit), "beta\\[")])
# tau <- matrix(tau, nrow = length(lambda))

if(suppressWarnings(str_detect(evaluate::evaluate("sdat$lv.par[,2]")[[2]], "Error"))) {
  sdat$lv.par <- cbind(sdat$lv.par[,1], 0)
}

comb_lambda <- rbind(c(0, sdat$lv.par[,1]),lambda)
comb_tau <- rbind(c(0, sdat$lv.par[,2]), tau)


eta <- df.fit %>%
  select(chain, matches("^eta")) %>%
  summarise_all(mean)
comb_eta <- rbind(c(0, sdat$theta), eta)

eta <- df.fit %>%
  select(chain, matches("^eta")) %>%
  summarise_all(mean) %>%
  select(-chain) %>%
  matrix(., nrow = 500, ncol = 2)

comb_eta <- cbind(sdat$theta, eta)

apply(comb_eta, 2, function(x) mean(unlist(x)))
apply(comb_eta, 2, function(x) var(unlist(x)))


plot(unlist(comb_eta[,1]), unlist(comb_eta[,3]))
plot(unlist(comb_eta[,2]), unlist(comb_eta[,4]))
# plot(unlist(comb_eta[1,]), unlist(comb_eta[4,]))
# plot(unlist(comb_eta[1,]), unlist(comb_eta[5,]))

#   eta <- data.frame(
#     est_eta = colMeans(df.fit[str_detect(names(df.fit), "^eta")]),
#     pop_eta = sdat$true_eta)

# mu.eta <- c(pop_mean = mean(eta[,2]), stan.mean = mean(eta[,1]), mueta = colMeans(df.fit[str_detect(names(df.fit), "muEta")]))

# plot(eta[,"est_eta"],eta[,"pop_eta"])
# cor(eta[,"est_eta"],eta[,"pop_eta"])
# main effect
true_data <- data.frame(Y = stan_dt$Y, X1 = stan_dt$X[,1], X2 = stan_dt$X[,2], eta = sdat$theta, Z = stan_dt$Z)

etas <- names(true_data)[str_detect(names(true_data), "eta")]

etapart<- paste(etas, collapse = "+")
etazpart <- paste(paste0(etas, "*Z"), collapse = "+")
true_form <- as.formula(glue::glue("Y ~ Z + {etapart} + {etazpart} + X1 + X2"))
true_param <- lm(true_form, data = true_data)
true_param <- coefficients(true_param)


true_param1 <- lm(eta ~ X1 + X2, data = true_data)
true_param1 <- coefficients(true_param1)

true_param <- c(0, true_param, true_param1[2:3])

names(true_param) <- c("chain","b00", "b0", "a1", "by1","by2", "b1", "bu1","bu2")
true_param <- true_param[c("chain", "b00", "b0", "b1", "a1", "by1","by2", "bu1","bu2")]

est_param <- df.fit %>%
  select(chain, matches("^b00|^b0|b1|a1|betaY|betaU")) %>%
  summarise_all(mean) %>%
  set_names(c("chain","bu1","bu2","by1","by2","b00","a1","b0","b1")) %>%
  select(chain, b00, b0, b1, a1, by1, by2, bu1, bu2)

est_param <- df.fit %>%
  select(matches("^b00|^b0|b1|a1|betaY|betaU")) %>%
  summarise_all(mean) %>%
  set_names(c("chain","bu1","bu2","by1","by2","b00","a1","b0","b1")) %>%
  select(chain, b00, b0, b1, a1, by1, by2, bu1, bu2)

# est_param <- data.frame(
#   b00  = colMeans(df.fit[str_detect(names(df.fit), "b00$")]),
#   b0  = colMeans(df.fit[str_detect(names(df.fit), "b0$")]),
#   b1  = colMeans(df.fit[str_detect(names(df.fit), "b1$")]),
#   a1  = colMeans(df.fit[str_detect(names(df.fit), "a1$")]),
#   by1  = colMeans(df.fit[str_detect(names(df.fit), "betaY")])[1],
#   by2  = colMeans(df.fit[str_detect(names(df.fit), "betaY")])[2],
#   bu1  = colMeans(df.fit[str_detect(names(df.fit), "betaU")])[1],
#   bu2  = colMeans(df.fit[str_detect(names(df.fit), "betaU")])[2]
# )

flps_param <- rbind(true = true_param, est = est_param)

# param_list[[i]] <- list(model_type = model_type, lv_param = lv_param, eta_param = eta, mu.eta = mu.eta, flps_param = flps_param)

eta_bias <-
  comb_eta %>%
  gather("etan", "eta_est", -chain) %>%
  mutate(chain = rep(0:4, 500)) %>%
  arrange(chain) %>%
  mutate(
    true_eta = rep(abs(sdat$true_eta), 5),
    eta_est = abs(eta_est)
  ) %>%
  group_by(chain) %>%
  summarise(
    bias = mean(eta_est - true_eta),
    rmse = sqrt(mean((eta_est - true_eta)^2)),
  )

param_list[[i]] <-
  list(model_type = paste0(model_type, "_", scaley),
       comb_lambda = comb_lambda,
       comb_tau = comb_tau,
       eta_param = comb_eta,
       flps_param = flps_param,
       eta_bias = eta_bias)
