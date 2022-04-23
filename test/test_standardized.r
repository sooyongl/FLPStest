# rm(list = ls())
library(tidyverse)
library(foreach)
library(lavaan)

res <- foreach(i = 1:1000, .combine = 'rbind') %do% {
  sim_condition <- list(
    N       = 1000, # sample size
    R2Y     = 0.2, # 0.1 0.2 0.5
    R2eta   = 0.5, # 0.2 0.5 0.75
    omega   = round(runif(1, 0.1, 0.3),3),
    tau0    = round(runif(1, 0.2, 0.4),3),
    tau1    = round(runif(1, -0.2, -0.1),3),
    linear  = T,
    ydist   = 'n',
    lambda  = 0.6,
    nsec    = 10,
    nfac    = 1,
    lvmodel = 'rasch'
  )

  sdat <- do.call("makeDat", sim_condition)
  N      <- sdat$N
  lambda <- sdat$lambda
  nsec   <- sdat$nsec
  eta <- sdat$theta

  Y <- sdat$stan_dt$Y
  Z <- sdat$stan_dt$Z
  X <- sdat$stan_dt$X
  int = Z*eta

  # sdat$omega
  # sdat$tau0
  # sdat$tau1

  ncov <- ncol(X)
  xname <- paste(paste0("X",1:ncov), collapse = " ")

  dat <- data.frame(y=Y, z=Z, X, eta, int)

  # lm(y ~ x1 + x2 + z + eta + int, data = dat)

  fit <- lavaan::sem(
    model = "
  y ~ x1 + x2 + z + eta + int

  x1 ~~ x2 + z + eta + int
  x2 ~~ z + eta + int
  z ~~ eta + int
  eta ~~ int
  ",
  data = dat,
  meanstructure = T
  )

  parameterestimates(fit, standardized = T) %>%
    filter(lhs == "y", op == "~") %>%
    select(rhs, est, std.all)
}


res %>%
  group_by(rhs) %>%
  summarise(
    meane = mean(est),
    means = mean(std.all)
  )

res %>%
  filter(rhs %in% c("z","eta" ,"int")) %>%
  ggplot(aes(x = std.all)) +
  geom_density() +
  facet_wrap(. ~ rhs)

# cov(dat)
# fitted(fit)
# lavInspect(fit, "est")




# summary(fit, standardized = T)
#
# -0.185 * (sd(int)/sd(Y))
# 0.6 * (sd(Z)/sd(Y))



