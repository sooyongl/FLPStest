# rm(list= ls()); gc()
library(tidyverse); library(rstan);library(lavaan); library(mirt)

eta.dt <- matrix(rnorm(100))
ipar.dt <- genIRTpar(20, 4, "grm")

wide.dt <- genIRTdt(lvmodel="grm",
         eta = eta.dt,
         ipar = ipar.dt) %>%
  data.frame(id = 1:nrow(.))

grm.mirt <- mirt(
  data = wide.dt[,1:20],
  model = paste0("F = 1-",20),
  itemtype = "graded")
coef(grm.mirt, simplify = T)

long.dt <- wide.dt %>% gather("item","y", -id) %>%
  mutate(item = as.numeric(str_remove(item, pattern = "Item_")))

grad <- long.dt$y

nsecWorked <- nrow(long.dt)
nstud <- nrow(wide.dt)
nsec <- ncol(wide.dt)-1

studentM <- long.dt$id
section <- long.dt$item

max_k <- max(grad) + 1

dt <- list(
  nsecWorked = nsecWorked,
  nstud = nstud,
  nsec = nsec,
  max_k = max_k,
  studentM = studentM,
  section = section,
  grad = grad
)


stan_model <- paste(read_lines("test/test_lv_model/stan/GGRM.stan"),
                    collapse = "\n")
cat(stan_model)

grm.stan <- rstan::stan(
  model_code = stan_model,
  data = dt,
  iter = 4000,
  cores = 1,
  chains = 1
)
grm.stan <- as.data.frame(grm.stan)

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
