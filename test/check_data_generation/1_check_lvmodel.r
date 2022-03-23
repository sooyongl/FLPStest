# rm(list = ls())
library(rstan)
library(foreach)
library(tidyverse)
library(doParallel)
library(Rmpi)

for(i in fs::dir_ls("R", regexp = "r$")) source(i);rm(i)
source_funs <- ls()

# core setting ------------------------------------------------------------
# cl <- getMPIcluster()
# doParallel::registerDoParallel(cl)

n_cores <- detectCores() - 2; print(n_cores)
cl <- parallel::makeCluster(n_cores)
doSNOW::registerDoSNOW(cl)

lvmodels = c("2pl", "grm", "gpcm")

# 2PL ---------------------------------------------------------------------
oo <- foreach(
  i = 1:100,
  .errorhandling = 'pass',
  .export = c(source_funs, "str_detect")
) %dopar% {
  # generate data ------------------------------------------
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

  pop_ipars <- sdat$lv.par

  # check LV part -----------------------------------------
  est_ipars <- check_lv(
    dat = sdat$lv.resp,
    # covdata = NULL,
    covdata = data.frame(sdat$x),
    lvmodel = sdat$lvmodel,
    nfac = sdat$nfac,
    IRTpars = F #ifelse(sdat$lvmodel=="2pl", T, F)
  )

  # mean(est_ipars$items.est$a - pop_ipars[,1])
  # mean(est_ipars$items.est$d - pop_ipars[,2])

  par_measure <- data.frame(est_a = est_ipars$items.est$a, pop_a = pop_ipars[,1],
                            est_b = est_ipars$items.est$d, pop_b = pop_ipars[,2])


  pop_struc <- c(0, sdat$tau0, sdat$tau1, sdat$omega, 1, 0.5, -1, 0.5)
  est_struc <- check_flps(sdat$stan_dt)

  a1 <- data.frame(t(rbind(est_struc, pop_struc)))
  par_struc <- data.frame(cbind(par_name = rownames(a1), a1))

  list(par_measure = par_measure, par_struc = par_struc)
}
stopCluster(cl)

par_measure <- lapply(oo, function(x) x$par_measure)
par_measure <- bind_rows(par_measure, .id = "rep")

par_measure %>%
  ggplot() +
  geom_point(aes(est_a, pop_a)) +
  geom_abline(slope = 1, intercept = 0)

par_measure %>%
  ggplot() +
  geom_point(aes(est_b, pop_b)) +
  geom_abline(slope = 1, intercept = 0)

par_struc <- lapply(oo, function(x) x$par_struc)
par_struc <- bind_rows(par_struc, .id = "rep")
par_struc <- par_struc %>% mutate(err = est_struc - pop_struc)

par_struc %>%
  ggplot(aes(x = par_name, y = err)) +
  # geom_boxplot() +
  geom_violin(fill = "skyblue", alpha = 0.5, color = NA) +
  ggforce::geom_sina(size = 1)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# GRM ---------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
oo <- foreach(
  i = 1:100,
  .errorhandling = 'pass',
  .export = c(source_funs, "str_detect")
) %dopar% {
  # generate data ------------------------------------------
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

  pop_ipars <- sdat$lv.par

  # check LV part -----------------------------------------
  est_ipars <- check_lv(
    dat = sdat$lv.resp,
    # covdata = NULL,
    covdata = data.frame(sdat$x),
    lvmodel = sdat$lvmodel,
    nfac = sdat$nfac,
    IRTpars = F #ifelse(sdat$lvmodel=="2pl", T, F)
  )

  # mean(est_ipars$items.est$a - pop_ipars[,1])
  # mean(est_ipars$items.est$d - pop_ipars[,2])

  par_measure <- data.frame(est_a = est_ipars$items.est[,1],
                            pop_a = pop_ipars[,1],
                            est = est_ipars$items.est[,-1],
                            pop = pop_ipars[,-1])


  pop_struc <- c(0, sdat$tau0, sdat$tau1, sdat$omega, 1, 0.5, -1, 0.5)
  est_struc <- check_flps(sdat$stan_dt)

  a1 <- data.frame(t(rbind(est_struc, pop_struc)))
  par_struc <- data.frame(cbind(par_name = rownames(a1), a1))

  list(par_measure = par_measure, par_struc = par_struc)
}
stopCluster(cl)

par_measure <- lapply(oo, function(x) x$par_measure)
par_measure <- bind_rows(par_measure, .id = "rep")

apar <- par_measure %>% select(starts_with("est_a"), starts_with("pop_a"))
dpar <- par_measure %>% select(starts_with("est.d"), starts_with("pop.d"))


ae <- apar[1] %>% gather("par_name", "est") %>%
  mutate(par_name = str_remove(par_name, "est_"))
ap <- apar[2] %>% gather("par_name", "pop") %>%
  mutate(par_name = str_remove(par_name, "pop_"))

ap <- ae %>% bind_cols(ap %>% select(-par_name))

de <- dpar[,1:3] %>% gather("par_name", "est") %>%
  mutate(par_name = str_remove(par_name, "est."))
dp <- dpar[,4:6] %>% gather("par_name", "pop") %>%
  mutate(par_name = str_remove(par_name, "pop."))

dp <- de %>% bind_cols(dp %>% select(-par_name))

par_measure <- bind_rows(ap, dp)

par_measure %>%
  ggplot() +
  geom_point(aes(est, pop), alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(. ~ par_name)

par_struc <- lapply(oo, function(x) x$par_struc)
par_struc <- bind_rows(par_struc, .id = "rep")
par_struc <- par_struc %>% mutate(err = est_struc - pop_struc)

par_struc %>%
  ggplot(aes(x = par_name, y = err)) +
  # geom_boxplot() +
  geom_violin(fill = "skyblue", alpha = 0.5, color = NA) +
  ggforce::geom_sina(size = 1)



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# GPCM---------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
oo <- foreach(
  i = 1:100,
  .errorhandling = 'pass',
  .export = c(source_funs, "str_detect")
) %dopar% {
  # generate data ------------------------------------------
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

  pop_ipars <- sdat$lv.par

  # check LV part -----------------------------------------
  est_ipars <- check_lv(
    dat = sdat$lv.resp,
    # covdata = NULL,
    covdata = data.frame(sdat$x),
    lvmodel = sdat$lvmodel,
    nfac = sdat$nfac,
    IRTpars = ifelse(sdat$lvmodel=="gpcm", T, F)
  )

  # mean(est_ipars$items.est$a - pop_ipars[,1])
  # mean(est_ipars$items.est$d - pop_ipars[,2])

  par_measure <- data.frame(est_a = est_ipars$items.est[,1],
                            pop_a = pop_ipars[,1],
                            est = est_ipars$items.est[,-1],
                            pop = pop_ipars[,-1])


  pop_struc <- c(0, sdat$tau0, sdat$tau1, sdat$omega, 1, 0.5, -1, 0.5)
  est_struc <- check_flps(sdat$stan_dt)

  a1 <- data.frame(t(rbind(est_struc, pop_struc)))
  par_struc <- data.frame(cbind(par_name = rownames(a1), a1))

  list(par_measure = par_measure, par_struc = par_struc)
}
stopCluster(cl)

par_measure <- lapply(oo, function(x) x$par_measure)
par_measure <- bind_rows(par_measure, .id = "rep")

apar <- par_measure %>% select(starts_with("est_a"), starts_with("pop_a"))
dpar <- par_measure %>% select(starts_with("est.b"), starts_with("pop.d"))


ae <- apar[1] %>% gather("par_name", "est") %>%
  mutate(par_name = str_remove(par_name, "est_"))
ap <- apar[2] %>% gather("par_name", "pop") %>%
  mutate(par_name = str_remove(par_name, "pop_"))

ap <- ae %>% bind_cols(ap %>% select(-par_name))

de <- dpar[,1:3] %>% gather("par_name", "est") %>%
  mutate(par_name = str_remove(par_name, "est."))
dp <- dpar[,4:6] %>% gather("par_name", "pop") %>%
  mutate(par_name = str_remove(par_name, "pop."))

dp <- de %>% bind_cols(dp %>% select(-par_name))

par_measure <- bind_rows(ap, dp)

par_measure %>%
  ggplot() +
  geom_point(aes(est, pop), alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(. ~ par_name)


par_struc <- lapply(oo, function(x) x$par_struc)
par_struc <- bind_rows(par_struc, .id = "rep")
par_struc <- par_struc %>% mutate(err = est_struc - pop_struc)

par_struc %>%
  ggplot(aes(x = par_name, y = err)) +
  # geom_boxplot() +
  geom_violin(fill = "skyblue", alpha = 0.5, color = NA) +
  ggforce::geom_sina(size = 1)












