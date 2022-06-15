# rm(list= ls()); gc()
library(tidyverse); library(rstan);library(lavaan); library(FLPS)

# CFA ---------------------------------------------------------------------
true_eta <- rnorm(500)
info <- FLPS:::parsForLVM(theta = true_eta, nsec  = 10, data_type = "sem")
data_info <- FLPS:::generate.sem(info)

grad <- data_info$resp
idx <- which(!is.na(grad), arr.ind = T)

nsecWorked <- nrow(idx)
nstud <- nrow(grad)
nsec <- ncol(grad)

studentM <- idx[,1]
section <- idx[,2]

grad <- sapply(1:dim(idx)[1], function(n) grad[idx[n,1], idx[n,2]] )

dt <- list(
  nsecWorked = nsecWorked,
  nstud = nstud,
  nsec = nsec,
  studentM = studentM,
  section = section,
  grad = grad
)

pop.lam <- data_info$lv.par


lavaan.lam1 <- cfa("F =~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10",
                   data = data.frame(data_info$resp)) %>%
  lavInspect(what = "est") %>% .[c("lambda","psi")]
lavaan.var1 <- cfa("F =~ NA*X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10
    F ~~ 1*F
    ",
    data = data.frame(data_info$resp)) %>%
  lavInspect(what = "est") %>% .$lambda


stan_model <- paste(read_lines("test/test_lv_model/stan/CFA.stan"),
                    collapse = "\n")
cat(stan_model)
cfa.fit.lam1 <- rstan::stan(
  model_code = stan_model,
  data = dt,
  iter = 4000,
  cores = 1,
  chains = 3
)

# cleaning ----------------------------------------------------------------
cfa.fit.lam1 <- as.data.frame(cfa.fit.lam1)

stan.lam1 <- colMeans(cfa.fit.lam1[str_detect(names(cfa.fit.lam1), "lambda1")])

data.frame(pop.lam,
           lavaan.lam1 = unname(lavaan.lam1$lambda),
           lavaan.var1 = unname(lavaan.var1),
           stan.lam1)

eta <- cbind(est_eta = colMeans(cfa.fit.lam1[str_detect(names(cfa.fit.lam1), "eta")]), pop_eta = true_eta)

c(pop_mean = mean(eta[,2]), stan.mean = mean(eta[,1]))

c(pop_var = var(eta[,2]), lavaan_var = lavaan.lam1$psi, stan.var = var(eta[,1]))
plot(eta[,"est_eta"],eta[,"pop_eta"])

# multiple factor --------------------------------------------------------------
eta <- MASS::mvrnorm(
  n=100,
  mu=rep(0, 2),
  Sigma = matrix(c(1,0,0,1), ncol = 2)
)

lambda <- matrix(c(1,1,1,1,0,0,0,
                   0,0,0,0,1,1,1), ncol = 2)

first_item <- apply(lambda, 2, function(x) {which(x == 1)[1]})
first_item_idx <- rep(0,nrow(lambda))
first_item_idx[first_item] <- 1

resi <- MASS::mvrnorm(
  n=100,
  mu=rep(0, 7),
  Sigma = diag(0.5, 7)
)

dt <- eta%*%t(lambda) + resi

# lavaan.lam1 <- cfa("F1 =~ X1 + X2 + X3 + X4
#                    F2 =~ X5 + X6 + X7",
#                    data = data.frame(dt))
# summary(lavaan.lam1)

long.dt <- data.frame(dt, id = 1:100) %>%
  gather("item_idx", "grad", -id) %>%
  mutate(
    item_idx = as.numeric(str_remove(item_idx,"X"))
  )

dt.stan <- list(
  nsecWorked = nrow(long.dt),
  nsec  = nrow(lambda),
  nfac = ncol(lambda),
  nstud = nrow(dt),
  section    = long.dt$item_idx,
  factoridx  = lambda,
  firstitem = first_item_idx,
  studentM  = long.dt$id,
  grad = long.dt$grad)

stan_model <- paste(read_lines("test/test_lv_model/stan/IRT_multivariate.stan"),
                    collapse = "\n")
cat(stan_model)
cfa.fit.lam1 <- rstan::stan(
  model_code = stan_model,
  data = dt.stan,
  iter = 4000,
  cores = 1,
  chains = 1
)


# cleaning ----------------------------------------------------------------
cfa.fit.lam1 <- as.data.frame(cfa.fit.lam1)

cfa.fit.lam1 %>%
  select(matches("^lambda\\[")) %>%
  summarise_all(mean) %>%
  matrix(., ncol = 2)

eta.stan <- cfa.fit.lam1 %>%
  select(matches("^eta\\[")) %>%
  summarise_all(mean) %>%
  matrix(., ncol = 2)
eta.stan <- as.numeric(eta.stan) %>% matrix(., ncol=2)
cov(eta.stan)
cov(eta)










