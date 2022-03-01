# 0302 --------------------------------------------------------------------
library(coda)
library(rstan)
library(tidyverse)
library(ggmcmc)
library(bayesplot)
library(ggmcmc)
# log_lik <- loo::extract_log_lik(fit, parameter_name = st_pars)
# loo::waic(log_lik)

pairs_stan <- function(chain, stan_model, pars) {
  energy <- as.matrix(sapply(get_sampler_params(stan_model, inc_warmup = F),
                             function(x) x[,"energy__"]))
  pars <- rstan::extract(stan_model, pars = pars, permuted = F)
  df <- data.frame(energy[,chain], pars[,chain,])
  names(df)[1] <- "energy"
  GGally::ggpairs(df, title = paste0("Chain", chain),
                  lower = list(continuous = GGally::wrap("points", alpha = 0.2)))
}

list.files("results")
# res <- readRDS("results/compare_chol_univ.rds")
# res <- readRDS("results/1000_50_rasch_TRUE_3.rds")

rdsname <- "switched_3"

# res <- readRDS("results/test_univ_switched_1.rds") # 6000 2000 2000 people
# res <- readRDS("results/test_univ_switched_2.rds") # 6000 2000 1000 people
res <- readRDS("results/test_univ_switched_3.rds") # 10000 3000 1000 people
# fit <- res$fit_univ

fit <- res$fit
sdat <- res$sdat

a11 <- sdat$omega
b0  <- sdat$tau0
b11 <- sdat$tau1
true_param <- c(-1, 0.5, 1.0, 0.5, 0, a11, b0, b11)
names(true_param) <- c("bu11","bu12","by1","by2","b00","a11", "b0", "b11")

# list_of_draws <- rstan::extract(fit); print(names(list_of_draws))

st_pars <- c("betaU","betaY","b00","a1","b0","b1")
ms_pars <- c("lambda[2,1]","lambda[3,1]","lambda[4,1]","lambda[5,1]")

est_param <- cbind(true_param, rstan::summary(fit, pars = st_pars)$summary)
param_table <- round(est_param, 3)

ggs_fit <- ggs(fit)

# strucparam_trace <- rstan::traceplot(object = fit, pars = st_pars)
# measuparam_trace <- rstan::traceplot(object = fit, pars = ms_pars)

b0_trace <- ggs_traceplot(ggs_fit, family = c("b0"))
ggsave(file.path("report/figure",paste0(rdsname, "_b0_trace.png")),
       b0_trace)

b1_trace <- ggs_traceplot(ggs_fit, family = c("b1"))
ggsave(file.path("report/figure",paste0(rdsname, "_b1_trace.png")),
       b1_trace)

a1_trace <- ggs_traceplot(ggs_fit, family = c("a1"))
ggsave(file.path("report/figure",paste0(rdsname, "_a1_trace.png")),
       a1_trace)

b0_auto <- ggmcmc(ggs_fit, file=NULL, family = c("b0"),
       plot="ggs_autocorrelation")
ggsave(file.path("report/figure",paste0(rdsname, "_b0_auto.png")),
       b0_auto)

b1_auto <- ggmcmc(ggs_fit, file=NULL, family = "b1",
       plot="ggs_autocorrelation")
ggsave(file.path("report/figure",paste0(rdsname, "_b1_auto.png")),
       b1_auto)

a1_auto <- ggmcmc(ggs_fit, file=NULL, family = "a1",
       plot="ggs_autocorrelation")
ggsave(file.path("report/figure",paste0(rdsname, "_a1_auto.png")),
       a1_auto)

pair_plot_chain1 <- pairs_stan(1, fit, c("b00","a1","b0","b1"))
ggsave(file.path("report/figure",
                 paste0(rdsname, "_pair_plot_chain1.png")),
       pair_plot_chain1)

pair_plot_chain2 <- pairs_stan(2, fit, c("b00","a1","b0","b1"))
ggsave(file.path("report/figure",
                 paste0(rdsname, "_pair_plot_chain2.png")),
       pair_plot_chain2)

pair_plot_chain3 <- pairs_stan(3, fit, c("b00","a1","b0","b1"))
ggsave(file.path("report/figure",
                 paste0(rdsname, "_pair_plot_chain3.png")),
       pair_plot_chain3)

pair_plot_chain4 <- pairs_stan(4, fit, c("b00","a1","b0","b1"))
ggsave(file.path("report/figure",
                 paste0(rdsname, "_pair_plot_chain4.png")),
       pair_plot_chain4)


mcmc_object <- As.mcmc.list(fit, pars = c("b00","a1","b0","b1"))

# effectiveSize(mcmc_object) # The effective sample size (ESS) measures the amount by which autocorrelation in samples increases uncertainty (standard errors) relative to an independent sample
gew <- geweke.diag(mcmc_object,frac1=.1,frac2=.5);
hei <- heidel.diag(mcmc_object);
gel <- gelman.diag(mcmc_object)


save(param_table,gew, hei, gel,
     file = file.path("report/rds",
                      paste0("0301_univ_",rdsname, ".Rdata")))



