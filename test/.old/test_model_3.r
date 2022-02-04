rm(list = ls())
library(rstan); library(tidyverse)
for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)
source_funs <- ls()

memory.limit(50000)
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
  lvmodel="grm")

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
setup_data <- setup(sim_condition, 3)
sim_condition  <- setup_data$sim_condition
stan_file_name <- setup_data$stan_file_name

names(sim_condition)
args(makeDat)

sdat <- do.call("makeDat", sim_condition)

# run FLPS ------------------------
names(sdat$stan_dt)
sdat$stan_dt$grad
fit <- rstan::stan(
  stan_file_name,
  data = sdat$stan_dt,
  iter = 5000,
  warmup = 2000,
  chains = 1,
  open_progress = F
)


# save --------------------------------------------------------------------
res <- list(fit = fit, sdat = sdat)
saveRDS(res, paste0("results/res_grmmulti_tau1.rds"))


# cleaning ----------------------------------------------------------------
library(rstan); library(tidyverse)
res_list <- fs::dir_ls("results")
res_list <- res_list[str_detect(res_list, "_tau1")]
(res_list <- res_list[str_detect(res_list, "res_")])

(filename <- res_list[1])
res <- readRDS(filename)

fit <- res$fit
sdat <- res$sdat
N <- res$sdat$N
stan_dt <- res$sdat$stan_dt
nb <- max(res$sdat$grad) - min(res$sdat$grad)
nfac <- res$sdat$nfac

iter = fit@sim$iter - fit@sim$warmup
chain = fit@sim$chains

# item param
df.fit <- as.data.frame(fit)
df.fit$chain <- rep(c(1:chain), each = iter)
df.fit <- df.fit %>% group_by(chain)


lambda <- df.fit %>%
  select(matches("^lambda\\[")) %>%
  summarise_all(mean) %>%
  select(-chain) %>%
  matrix(., ncol = nfac)

tau <- df.fit %>%
  select(chain, matches("^tau\\[")) %>%
  summarise_all(mean) %>%
  select(-chain) %>%
  matrix(., ncol = nb)

comb_lambda <- cbind(sdat$lv.par[,1:nfac],lambda)
comb_lambda <- apply(comb_lambda, 2, as.numeric)
comb_tau    <- cbind(sdat$lv.par[,(nfac+1):ncol(sdat$lv.par)], tau)
comb_tau    <- apply(comb_tau, 2, as.numeric)

eta <- df.fit %>%
  select(chain, matches("^eta")) %>%
  summarise_all(mean) %>%
  select(-chain) %>%
  matrix(., ncol = nfac, nrow = N)

comb_eta <- cbind(sdat$theta, eta)
comb_eta <- apply(comb_eta, 2, as.numeric)

apply(comb_eta, 2, function(x) mean(unlist(x)))
apply(comb_eta, 2, function(x) var(unlist(x)))


plot(unlist(comb_eta[,1]), unlist(comb_eta[,3]))
plot(unlist(comb_eta[,2]), unlist(comb_eta[,4]))

# main effect ------------------------------------------
true_data <- data.frame(Y = stan_dt$Y, X1 = stan_dt$X[,1], X2 = stan_dt$X[,2], eta = sdat$theta, Z = stan_dt$Z)

etas <- names(true_data)[str_detect(names(true_data), "eta")]

etapart<- paste(etas, collapse = "+")
etazpart <- paste(paste0(etas, "*Z"), collapse = "+")
true_form <- as.formula(glue::glue("Y ~ Z + {etapart} + {etazpart} + X1 + X2"))
true_param <- lm(true_form, data = true_data)
true_param <- coefficients(true_param)

true_param1 <- lm(eta.eta1 ~ X1 + X2, data = true_data)
true_param2 <- lm(eta.eta2 ~ X1 + X2, data = true_data)
true_param12 <- c(coefficients(true_param1)[2:3],
                  coefficients(true_param2)[2:3])

true_param <- c(true_param, true_param12)

names(true_param) <- c("b00", "b0", "a11","a12", "by1","by2", "b11","b12", "bu11","bu12","bu21","bu22")
true_param <- true_param[c("b00", "b0", "b11","b12", "a11","a12", "by1","by2", "bu11","bu12","bu21","bu22")]

est_param <- df.fit %>%
  select(matches("^b00|^b0|b1|a1|betaY|betaU")) %>%
  summarise_all(mean) %>%
  select(-chain) %>%
  set_names(c("bu11","bu12","bu21","bu22","by1","by2","b00","a11","a12","b0","b11","b12")) %>%
  select(b00, b0, b11,b12, a11,a12, by1,by2,bu11,bu12,bu21,bu22)

flps_param <- bind_rows(true = true_param, est = est_param, .id = "type")

saveRDS(list(flps_param=flps_param,
             comb_lambda=comb_lambda,
             comb_tau=comb_tau,
             comb_eta=comb_eta), "results/cleaned_grm_lam.rds")


# -------------------------------------------------------------------------
file_list <- fs::dir_ls("results")
cleaned_res <- file_list[str_detect(file_list, "cleaned")]

for(i in c(1,5,9)) {

  # i = 5
  res1 <- readRDS(cleaned_res[i])
  res2 <- readRDS(cleaned_res[i + 1])
  res3 <- readRDS(cleaned_res[i + 2])
  res4 <- readRDS(cleaned_res[i + 3])
  
  bind_rows(res1$flps_param, res2$flps_param, 
            res3$flps_param,res4$flps_param,
            .id = "cond") %>%
    mutate(cond = case_when(cond == 1 ~ cleaned_res[i], 
                            cond == 2 ~ cleaned_res[i+1],
                            cond == 3 ~ cleaned_res[i+2],
                            TRUE ~  cleaned_res[i+3]))

  bind_rows(data.frame(res1$comb_lambda), data.frame(res2$comb_lambda), 
            data.frame(res3$comb_lambda), data.frame(res4$comb_lambda),
            .id = "cond") %>%
    mutate(cond = case_when(cond == 1 ~ cleaned_res[i], 
                            cond == 2 ~ cleaned_res[i+1],
                            cond == 3 ~ cleaned_res[i+2],
                            TRUE ~  cleaned_res[i+3]))

  bind_rows(data.frame(res1$comb_tau), data.frame(res2$comb_tau), 
            data.frame(res3$comb_tau),data.frame(res4$comb_tau),
            .id = "cond") %>%
    mutate(cond = case_when(cond == 1 ~ cleaned_res[i], 
                            cond == 2 ~ cleaned_res[i+1],
                            cond == 3 ~ cleaned_res[i+2],
                            TRUE ~  cleaned_res[i+3]))

  bind_rows(data.frame(res1$comb_eta), data.frame(res2$comb_eta),
            data.frame(res3$comb_eta) ,data.frame(res4$comb_eta) ,
            .id = "cond") %>%
    mutate(cond = case_when(cond == 1 ~ cleaned_res[i], 
                            cond == 2 ~ cleaned_res[i+1],
                            cond == 3 ~ cleaned_res[i+2],
                            TRUE ~  cleaned_res[i+3]))

}









