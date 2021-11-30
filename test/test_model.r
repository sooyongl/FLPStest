rm(list = ls())
library(rstan); library(tidyverse)
for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)
source_funs <- ls()

memory.limit(50000)
rstan_options(auto_write = TRUE)

file_list <- c(
  "inst/stan/psIRT_scaling.stan",
  "inst/stan/psGPCM_scaling.stan",
  "inst/stan/psSEM_scaling.stan"
)
lvmodel_list <- c("2PL", "GPCM", "sem")

# model ---------------------------------------------------------
stan_file_name <- file_list[2]
lvmodel <- lvmodel_list[2]

for(i in 1:10) {
  # i <- 1
  # -------------------------------------------------------------------------
  parsFromMod <- list(
    N = 500, # sample size
    R2Y = 0.2,
    omega = 0.2,  # a1
    tau0 = 0.13,  # b0
    tau1 = -0.06, # b1
    lambda = 10,
    R2eta = 0.2,
    nsec = 20,
    lvmodel = lvmodel # tag for latent variable model
  )
  sdat <- do.call("makeDat", parsFromMod)
  fit <- stan(stan_file_name,data=sdat,iter=5000,warmup=2000,cores=1,chains=1)
  res <- list(fit = fit, sdat = sdat)
  saveRDS(res, paste0("results/res_", parsFromMod$lvmodel,"_",i ,".rds"))

  print(i)
}

# -------------------------------------------------------------------------
res_list <- fs::dir_ls("results", regexp = "rds$")

res_list <- res_list[str_detect(res_list, "GPCM")]

param_list <- vector("list", length(res_list))
for(i in 1:length(res_list)) {
  model_type <- str_split(res_list[i], "_", simplify = T)[2]
  res <- readRDS(res_list[i])

  fit <- res$fit
  sdat <- res$sdat

  # item param
  df.fit <- as.data.frame(fit)

  lambda <- colMeans(df.fit[str_detect(names(df.fit), "lambda\\[")])
  tau <- colMeans(df.fit[str_detect(names(df.fit), "tau\\[")])
  tau <- matrix(tau, nrow = length(lambda))

  lv_param <- cbind(sdat$lv.par,lam  = lambda, tau = tau)

  eta <- data.frame(
    est_eta = colMeans(df.fit[str_detect(names(df.fit), "^eta")]),
    pop_eta = sdat$true_eta)

  mu.eta <- c(pop_mean = mean(eta[,2]), stan.mean = mean(eta[,1]), mueta = colMeans(df.fit[str_detect(names(df.fit), "muEta")]))

  # plot(eta[,"est_eta"],eta[,"pop_eta"])
  # cor(eta[,"est_eta"],eta[,"pop_eta"])
  # main effect
  true_data <- data.frame(Y = sdat$Y, X1 = sdat$X[,1], X2 = sdat$X[,2], eta = sdat$true_eta, Z = sdat$Z)
  true_param <- lm(Y ~ Z + eta + eta*Z + X1 + X2, data = true_data)
  true_param <- coefficients(true_param)

  true_param1 <- lm(eta ~ X1 + X2, data = true_data)
  true_param1 <- coefficients(true_param1)

  true_param <- c(true_param, true_param1[2:3])

  names(true_param) <- c("b00", "b0", "a1", "by1","by2", "b1", "bu1","bu2")
  true_param <- true_param[c("b00", "b0", "b1", "a1", "by1","by2", "bu1","bu2")]

  est_param <- data.frame(
    b00  = colMeans(df.fit[str_detect(names(df.fit), "b00$")]),
    b0  = colMeans(df.fit[str_detect(names(df.fit), "b0$")]),
    b1  = colMeans(df.fit[str_detect(names(df.fit), "b1$")]),
    a1  = colMeans(df.fit[str_detect(names(df.fit), "a1$")]),
    by1  = colMeans(df.fit[str_detect(names(df.fit), "betaY")])[1],
    by2  = colMeans(df.fit[str_detect(names(df.fit), "betaY")])[2],
    bu1  = colMeans(df.fit[str_detect(names(df.fit), "betaU")])[1],
    bu2  = colMeans(df.fit[str_detect(names(df.fit), "betaU")])[2]
  )

  flps_param <- rbind(true = true_param, est = est_param)

  param_list[[i]] <- list(model_type = model_type, lv_param = lv_param, eta_param = eta, mu.eta = mu.eta, flps_param = flps_param)
}
param_list.gpcm <- param_list

param_list.sem <- param_list
param_list.2PL <- param_list

res_combined_1130 <- list(param_list.sem = param_list.sem,
                          param_list.2PL = param_list.2PL,
                          param_list.gpcm = param_list.gpcm)

saveRDS(res_combined_1130, "report/rds/res_combined_1130.rds")
