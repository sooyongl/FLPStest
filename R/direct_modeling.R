# The reviewer asks for a comparison between your proposed methods and simpler approaches, such as directly modeling the relationship between 𝑌, 𝑀, and 𝑍.

library(tidyverse)
library(blavaan)

res1 <- fs::dir_ls('D:\\FLPS\\results\\', regex = "rds$")
res2 <- fs::dir_ls('D:\\FLPS\\rerun\\', regex = "rds$")
res3 <- fs::dir_ls("D:\\FLPS\\results_poly", regex = "rds$")

a1 <- readRDS(res1[1])
sfit <- data.frame(summary(a1$fit)$summary)
sfit$par_name <- rownames(sfit)

sfit %>% filter(str_detect(par_name, "b0|b1"))


sdat <- a1$sdat

sdat$tau1

inp_data <- bind_cols(
  data.frame(
    Y = sdat$stan_dt$Y,
    Z = sdat$stan_dt$Z,
    sdat$stan_dt$X),
  data.frame(sdat$lv.resp)
) %>%
  mutate_at(vars(matches("Item_")), ~ if_else(Z == 0, NA_integer_, .x)) %>%
  mutate(
    M_item = rowMeans(across(matches("Item_")), na.rm = T),
    M_item = if_else(is.nan(M_item), NA, M_item),
    int_MZ = M_item * Z
  )

model = "Y ~ Z + M_item + int_MZ + x1 + x2;
        M_item ~ x1 + x2;

        Z ~~ x1 + x2;
        x1 ~~ x2;

        int_MZ ~~ 0*x1;
        int_MZ ~~ 0*x2;
        int_MZ ~~ 0*Z;
"

bfit <- bsem(
  model = model,
  data = inp_data,
  n.chains = 2,
  burnin = 2500,
  sample = 2500,
  fixed.x = F,
  dp = dpriors(nu = "normal(0,5)", beta = "normal(0,5)"))

summary(bfit)

# library(lavaan)
# sem(
#   model = model,
#   data = inp_data,
#   fixed.x = F, missing = "fiml")
#
#
#
bfit_multi <- rblimp::rblimp(
  model = (
    glue::glue("
        Y ~ Z  M_item  int_MZ  x1  x2;
        M_item ~ x1 x2;

        Z ~~ x1 x2;
        x1 ~~ x2;

        int_MZ ~~ x1@0;
        int_MZ ~~ x2@0;
        int_MZ ~~ Z@0;

    ")
  ),
data = inp_data,
burn = 2000,
iter = 5000,
chain = "2 processors 1;",
output = "default_median",
seed = 1)

best <- data.frame(bfit_multi@estimates)
best$par_name <- rownames(best)
best %>%
  filter(str_detect(par_name, "(Y ~ int_MZ|Y ~ Z)"))












