# rm(list = ls())
library(rstan)
library(foreach)
library(tidyverse)
library(doParallel)
# library(Rmpi)

setwd("C:/Users/lee/Box/git/FLPS")

source("test/check_data_generation/z_extracting_function.r")
for(i in fs::dir_ls("R", regexp = "r$")) source(i);rm(i)
source_funs <- ls()

stancode <- list.files("test/check_data_generation/test_stancode", full.names = T)

lv_stan_filename <- stancode[-grep("univ", stancode)]

# 2PL ---------------------------------------------------------------------
for(i in 1:10) {
  print(i)
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

  sdat <- do.call("makeDat", sim_condition)

  IRT_code <- lv_stan_filename[grep("IRT", lv_stan_filename)]

  fit <- rstan::stan(
    file   = IRT_code[grep("unif", IRT_code)],
    data   = sdat$stan_dt,
    chain  = 1,
    cores  = 1,
    iter   = 5000,
    warmup = 2000
  )

  o <- list(fit=fit, sdat=sdat)
  saveRDS(o, file.path("test/check_data_generation/results",paste0("unifztwo_2pl_", i, ".rds")))

  rm(fit)
  #
  # fit <- rstan::stan(
  #   file   = IRT_code[grep("normal", IRT_code)],
  #   data   = sdat$stan_dt,
  #   chain  = 1,
  #   cores  = 1,
  #   iter   = 5000,
  #   warmup = 2000
  # )
  # o <- list(fit=fit, sdat=sdat)
  # saveRDS(o, file.path("test/check_data_generation/results",paste0("normalztwo_2pl_", i, ".rds")))
  # rm(fit)

  # fit <- rstan::stan(
  #   file   = IRT_code[grep("logn", IRT_code)],
  #   data   = sdat$stan_dt,
  #   chain  = 1,
  #   cores  = 1,
  #   iter   = 5000,
  #   warmup = 2000
  # )
  # o <- list(fit=fit, sdat=sdat)
  # saveRDS(o, file.path("test/check_data_generation/results",paste0("lognztwo_2pl_", i, ".rds")))
  #
  # rm(fit)

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

# files <- str_subset(files, "comple")
# files <- files[grep("_2[0-9]|_3[0-9]|_4[0-9]\\.", files)]

results <- lapply(1:length(files), function(x) {

  aaa <- str_split(files[x], "/", simplify = T)[,4]
  aaa <- str_split(aaa, ".rds", simplify = T)[,1]

  a1 <- readRDS(files[x])
  a1 <- extract_pars(fit = a1$fit, sdat = a1$sdat, str_detect(files[x], "two"))

  a1 <- append(aaa, a1)
  a1
})
saveRDS(results, "test/check_data_generation/results.rds")

results <- readRDS("test/check_data_generation/results.rds")
results00 <- readRDS("test/check_data_generation/results000000.rds")

saveRDS(c(results, results00), "test/check_data_generation/results1.rds")


cond_df <- map_chr(results,  ~ .x[[1]]) %>% tibble(.) %>%
  mutate(rep = row_number()) %>%
  mutate(rep = as.character(rep)) %>%
  set_names(c("condition", "rep"))

eta_df <- map_df(results,  ~ .x$eta_df, .id = "rep")
sum_eta <- eta_df %>%
  left_join(cond_df, by ="rep") %>%
  group_by(condition) %>%
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
  left_join(cond_df, by ="rep") %>%
  group_by(condition) %>%
  summarise(
    true_mean = mean(true_lam), est_mean = mean(est_mean), err = est_mean - true_mean) %>%
  mutate(par_name = "lambda")

tau_df <- map_df(results,  ~ .x$tau_df, .id = "rep")
sum_tau <- tau_df %>%
  left_join(cond_df, by ="rep") %>%
  group_by(condition) %>%
  summarise(
    true_mean = mean(true_tau), est_mean = mean(est_mean), err = est_mean - true_mean) %>%
  mutate(par_name = "tau")

struct_df <- map_df(results,  ~ .x$struct_df, .id = "rep")

struct_df %>%
  left_join(., cond_df, by = c("rep")) %>%
  mutate(err = est_mean - true_struc) %>%
  select(condition, true_mean = true_struc, est_mean, err, par_name) %>%
  bind_rows(sum_eta, sum_lam, sum_tau) %>%

  separate(condition, c("condition", "a"), sep = "_2pl_") %>%
  select(-a, -rep) %>%

  mutate() %>%

  # plotting(.) +
  ggplot(aes(x = par_name, y = err)) +
  geom_violin(
    trim=F,
    fill = "skyblue", alpha = 0.5, color = NA) +
  ggforce::geom_sina(size = 2) +
  geom_hline(yintercept = 0) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    alpha = 0.8,
    fill = "red"
  ) +
  facet_wrap(. ~ condition) +
  scale_y_continuous(limits = c(-1, 1), n.breaks = 10)



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
