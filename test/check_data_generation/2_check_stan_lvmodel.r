# rm(list = ls())
library(rstan)
library(foreach)
library(tidyverse)
library(doParallel)
# library(Rmpi)
source("test/check_data_generation/z_extracting_function.r")
for(i in fs::dir_ls("R", regexp = "r$")) source(i);rm(i)
source_funs <- ls()

stancode <- list.files("test/check_data_generation/test_stancode", full.names = T)

lv_stan_filename <- stancode[-grep("univ", stancode)]

# 2PL ---------------------------------------------------------------------
for(i in 11:20) {
  sim_condition <- list(
    N       = 1000, # sample size
    R2Y     = 0.3,   # 0.1 0.2 0.5
    R2eta   = 0.5,   # 0.2 0.5 0.75
    omega   = 0.2,   # round(runif(1, 0.1, 0.3),3),
    tau0    = 0.3,   # round(runif(1, 0.2, 0.4),3),
    tau1    = -0.15, # round(runif(1, -0.2, -0.1),3),
    linear  = T,
    ydist   = "n",
    lambda  = 0.6,
    nsec    = 20,
    nfac    = 1,
    lvmodel = "2pl"
  )

  sdat <- do.call("test_makeDat", sim_condition)

  fit <- rstan::stan(
    file   = lv_stan_filename[grep("IRT", lv_stan_filename)],
    data   = sdat$stan_dt,
    chain  = 1,
    cores  = 1,
    iter   = 5000,
    warmup = 2000
  )

  o <- list(fit=fit, sdat=sdat)

  saveRDS(o, file.path("test/check_data_generation/results",paste0("logn_2pl_", i, ".rds")))
  gc()
}

# res_par_2pl <- extract_pars(o$fit, o$sdat)
#
# res_par_2pl
# plot(res_par_2pl$eta_df[[2]],res_par_2pl$eta_df[[3]])
# mean(res_par_2pl$eta_df[[2]])
# mean(res_par_2pl$eta_df[[3]])
#
# var(res_par_2pl$eta_df[[2]])
# var(res_par_2pl$eta_df[[3]])
#
# rm(list = ls())
library(tidyverse)
source("test/check_data_generation/z_extracting_function.r")

files <- fs::dir_ls("test/check_data_generation/results")

# files <- files[grep("_2[0-9]|_3[0-9]|_4[0-9]\\.", files)]

results <- lapply(1:length(files), function(x) {

  xx <- str_split(files[x], "/", simplify = T)[1,4]
  xx <- str_split(xx, "_", simplify = T)[1,1]

  a1 <- readRDS(files[x])
  a2 <- extract_pars(fit = a1$fit, sdat = a1$sdat)
  a2$condition <- xx
  a2
}
)
saveRDS(results, "test/check_data_generation/results.rds")

condition <- map_chr(results,  ~ .x$condition, .id = "rep")

eta_df <- map_df(results,  ~ .x$eta_df, .id = "rep")

sum_eta <- eta_df %>%

  mutate(condition = rep(c("logn","normal"), each = nrow(eta_df)/2) ) %>%

  group_by(rep, condition) %>%
  summarise(
    true_mean = mean(true_eta), est_mean = mean(est_mean), err = est_mean - true_mean) %>%
  mutate(par_name = "eta")

# eta_df %>%
#   group_split(rep) %>%
#   map(., ~ cor(.x$true_eta, .x$est_mean))
#
# eta_df %>%
#   ggplot() +
#   geom_point(aes(true_eta, est_mean)) +
#   facet_wrap(~rep)

lambda_df <- map_df(results,  ~ .x$lambda_df, .id = "rep")
sum_lam <- lambda_df %>%

  mutate(condition = rep(c("logn","normal"), each = nrow(lambda_df)/2) ) %>%

  group_by(rep, condition) %>%
  summarise(
    true_mean = mean(true_lam), est_mean = mean(est_mean), err = est_mean - true_mean) %>%
  mutate(par_name = "lambda")

tau_df <- map_df(results,  ~ .x$tau_df, .id = "rep")
sum_tau <- tau_df %>%
  mutate(condition = rep(c("logn","normal"), each = nrow(tau_df)/2) ) %>%
  group_by(rep, condition) %>%
  summarise(
    true_mean = mean(true_tau), est_mean = mean(est_mean), err = est_mean - true_mean) %>%
  mutate(par_name = "tau")

struct_df <- map_df(results,  ~ .x$struct_df, .id = "rep")
struct_df <- struct_df %>%
  mutate(condition = rep(c("logn","normal"), each = nrow(struct_df)/2) ) %>%
  mutate(err = est_mean - true_struc) %>%
  select(rep, true_mean = true_struc, est_mean, err, par_name, condition) %>%
  bind_rows(sum_eta, sum_lam, sum_tau)
saveRDS(struct_df, "report/rds/0330_simple_IRT.rds")

struct_df %>%
  plotting(.) +
  facet_grid(.~condition) +
  scale_y_continuous(limits = c(-0.5, 0.5), n.breaks = 20) +
  theme_bw(base_size = 16)



# GRM ---------------------------------------------------------------------
sim_condition <- list(
  N       = 1000, # sample size
  R2Y     = 0.3,   # 0.1 0.2 0.5
  R2eta   = 0.5,   # 0.2 0.5 0.75
  omega   = 0.2,   # round(runif(1, 0.1, 0.3),3),
  tau0    = 0.3,   # round(runif(1, 0.2, 0.4),3),
  tau1    = -0.15, # round(runif(1, -0.2, -0.1),3),
  linear  = T,
  ydist   = "n",
  lambda  = 0.6,
  nsec    = 20,
  nfac    = 1,
  lvmodel = "grm"
)

sdat <- do.call("makeDat", sim_condition)

fit <- rstan::stan(
  file   = lv_stan_filename[grep("GRM", lv_stan_filename)],
  data   = sdat$stan_dt,
  chain  = 1,
  cores  = 1,
  iter   = 5000,
  warmup = 2000
)

o <- list(fit=fit, sdat=sdat)
saveRDS(o, file.path("test/check_data_generation/results","grm.rds"))
res_par_grm <- extract_pars(fit, sdat)

# GPCM ---------------------------------------------------------------------
sim_condition <- list(
  N       = 1000, # sample size
  R2Y     = 0.3,   # 0.1 0.2 0.5
  R2eta   = 0.5,   # 0.2 0.5 0.75
  omega   = 0.2,   # round(runif(1, 0.1, 0.3),3),
  tau0    = 0.3,   # round(runif(1, 0.2, 0.4),3),
  tau1    = -0.15, # round(runif(1, -0.2, -0.1),3),
  linear  = T,
  ydist   = "n",
  lambda  = 0.6,
  nsec    = 20,
  nfac    = 1,
  lvmodel = "gpcm"
)

sdat <- do.call("makeDat", sim_condition)

fit <- rstan::stan(
  file   = lv_stan_filename[grep("GPCM", lv_stan_filename)],
  data   = sdat$stan_dt,
  chain  = 1,
  cores  = 1,
  iter   = 5000,
  warmup = 2000
)

o <- list(fit=fit, sdat=sdat)
saveRDS(o, file.path("test/check_data_generation/results","gpcm.rds"))
res_par_gpcm <- extract_pars(fit, sdat)

res_par_gpcm$eta_df %>%
  mutate(trt = rep(c("trt","ctl"), 500)) %>%
  ggplot(aes(true_eta, est_mean, color = trt)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)
