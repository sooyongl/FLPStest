rm(list = ls())
library(tidyverse)
time_loading <- matrix(c(1,1,1,1,1,1, 0,1,2,3,4,5), ncol = 2)
lat_mat <- matrix(c(1, 0.1, 0.1, 1), ncol = 2)
res_mat <- diag(0.5, dim(time_loading)[1])

lat_mean <- c(3, 0.5)

obv_mean <- time_loading %*% matrix(lat_mean, nrow = 2)
obv_cov  <- time_loading %*% lat_mat %*% t(time_loading) + res_mat

lgm_data <- MASS::mvrnorm(n = 200, mu = obv_mean, Sigma = obv_cov, empirical = T)

long_data <- data.frame(lgm_data, id = 1:dim(lgm_data)[1]) %>%
  gather("time","response", -id) %>%
  mutate(
    time0 = 1,
    time1 =
           case_when(time == "X1" ~ 0,
                     time == "X2" ~ 1,
                     time == "X3" ~ 2,
                     time == "X4" ~ 3,
                     time == "X5" ~ 4,
                     time == "X6" ~ 5),
    time_idx =
      case_when(time == "X1" ~ 1,
                time == "X2" ~ 2,
                time == "X3" ~ 3,
                time == "X4" ~ 4,
                time == "X5" ~ 5,
                time == "X6" ~ 6))

stan_data <- list(

  N        = dim(long_data)[1],
  ntime    = dim(lgm_data)[2],
  nindi    = dim(lgm_data)[1],
  nfac     = 2,

  tidx     = long_data$time_idx,
  iidx     = long_data$id,

  time_loading    = cbind(long_data$time0, long_data$time1),
  response = long_data$response

)
library(lavaan)
growth(model =
                 "I =~ 1*X1 + 1*X2 + 1*X3 + 1*X4 + 1*X5 + 1*X6
                  S =~ 0*X1 + 1*X2 + 2*X3 + 3*X4 + 4*X5 + 5*X6",
               data = data.frame(lgm_data)) %>% summary()

stan_model <- paste(read_lines("test/test_lv_model/stan/LGM.stan"),
                    collapse = "\n")
cat(stan_model)
lgm.fit.lam1 <- rstan::stan(
  model_code = stan_model,
  data = stan_data,
  iter = 4000,
  cores = 1,
  chains = 1
)

stanc("longitudinal.stan")

lgm.fit.lam1 <- as.data.frame(lgm.fit.lam1)

stan.theta <- colMeans(lgm.fit.lam1[str_detect(names(lgm.fit.lam1), "thetas")])
colMeans(round(matrix(stan.theta, ncol = 2), 2))
round(apply(matrix(stan.theta, ncol = 2), 2, var), 2)

stan.theta <- colMeans(lgm.fit.lam1[str_detect(names(lgm.fit.lam1), "L")])

stan.theta <- colMeans(lgm.fit.lam1[str_detect(names(lgm.fit.lam1), "item_vars")])

stan.theta <- colMeans(lgm.fit.lam1[str_detect(names(lgm.fit.lam1), "A")])


