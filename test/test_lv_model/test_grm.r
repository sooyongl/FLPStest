library(rstan); library(tidyverse); library(foreach)
for(i in fs::dir_ls("R", regexp = "r$")) source(i);

# cond_list <- data.frame()
# for(i in 1:dim(cond_list)[1]) {
#
# }

# generate data ------------------------------------------
sim_condition <- list(
  N       = 2000, # sample size
  R2Y     = 0.2,
  R2eta   = 0.2,
  omega   = 0.2,  # a1
  tau0    = 0.4,  # b0
  tau1    = -0.2, # b1
  linear  = T,
  lambda  = 0.6,
  nsec    = 20,
  nfac    = 1,
  lvmodel ="grm"
)

sdat <- do.call("makeDat", sim_condition)

stan_model <- paste(read_lines("test/test_lv_model/stan/GGRM.stan"),
                    collapse = "\n")
cat(stan_model)

grm.stan <- rstan::stan(
  file = "test/test_lv_model/stan/GGRM.stan",
  # model_code = stan_model,
  data = sdat$stan_dt,
  iter = 4000,
  cores = 1,
  chains = 1
)
summary(grm.stan, pars = "alpha")
beta <- summary(grm.stan, pars = "beta")

matrix(beta$summary[,1], ncol = 3, byrow = T)
sdat$lvinfo$ipar
grm.stan <- as.data.frame(grm.stan)
# item parameter






ipar.dt
coef(grm.mirt, simplify = T)

grm.stan %>%
  select(matches("^alpha|^beta|^kap")) %>%
  summarise_all(mean)


# -------------------------------------------------------------------------

Y <- wide.dt[,1:20] + 1

n_person <- nrow(Y)
n_item <- ncol(Y)

K <- max(Y)

dt <- list(
  Y = Y,
  n_person = n_person,
  n_item = n_item,
  K = K
)

stan_model <- paste(read_lines("test/test_lv_model/stan/GGRM_ordered.stan"),collapse = "\n")
cat(stan_model)

ordered.stan <- rstan::stan(
  model_code = stan_model,
  data = dt,
  iter = 8000,
  cores = 1,
  chains = 1
)
ipar.dt
coef(grm.mirt, simplify = T)$items

ordered.stan <- as.data.frame(ordered.stan)
est.orstan <- ordered.stan %>%
  select(matches("^alpha\\[|^ka\\[")) %>%
  summarise_all(mean)

a <- est.orstan %>%
  select(matches("^alpha")) %>%
  t()
d <- est.orstan %>%
  select(matches("^ka")) %>%
  mutate_all( ~ -1*.) %>%
  matrix(., ncol = 3)
cbind(a, d)
