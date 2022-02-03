library(rstan); library(tidyverse)
for(i in fs::dir_ls("R", regexp = "r$")) source(i);

# generate data -----------------------------------------------------------
sim_condition <- list(
  N = 1000, # sample size
  R2Y = 0.2,
  omega = 0.2,  # a1
  tau0 = 0.4,  # b0
  tau1 = -0.2, # b1
  lambda = 0.6,
  R2eta = 0.2,
  linear = T,
  nsec = 20,
  nfac = 1,
  lvmodel="gpcm",
  lvinfo =
    list(
      ipar = round(genIRTpar(nitem = 20,
                             nfac = 1,
                             lvmodel = "gpcm"),3)
    )
)

sdat <- do.call("makeDat", sim_condition)
# sdat <- readRDS("test/test_sdata_0.rds")

stan_dt <- sdat$stan_dt

# check LV part -----------------------------------------------
check_lv(
  dat = sdat$lv.resp,
  covdata = NULL, #data.frame(sdat$x),
  lvmodel = "gpcm",
  nfac = 1,
  IRTpars = T
)
# item parameter
sdat$lvinfo$ipar

# check model part --------------------------------------------------------
check_flps(stan_dt)

# factor index
stan_dt$factoridx
stan_dt$firstitem

# run flps ----------------------------------------------------------------
fit <- rstan::stan(
  file = "inst/stan/psGPCM_multi.stan",
  data = stan_dt,
  iter = 6000,
  warmup = 2000,
  chains = 1,
  open_progress = F
)
o <- list(fit=fit, sdat=sdat)
# cleaning ----------------------------------------------------------------
saveRDS(o, "test/test_model_gpcm.rds")
o <- readRDS("test/test_model_0.rds")

res <- clean_temp(fit = o[[1]], sdat = o[[2]])
saveRDS(res, "test/test_cleaned_gpcm.rds")


data.frame(res$comb_lambda) %>%
  set_names(c("pop.a1","pop.a2","est.a1","est.a2")) %>%
  tibble()

data.frame(res$comb_tau[,c(1,3)]) %>%
  set_names(c("pop.b","est.b")) %>%
  tibble()

res$flps_param

colnames(res$comb_eta) <- c("pop.est1", "pop.est2","est.eta1","est.eta2")
round(cor(res$comb_eta),3)
mean(res$comb_eta[,1] - res$comb_eta[,3])
mean(res$comb_eta[,2] - res$comb_eta[,4])

mean((res$comb_eta[,1] - res$comb_eta[,3])^2)
mean((res$comb_eta[,2] - res$comb_eta[,4])^2)

data.frame(res$comb_eta[],id = 1:nrow(res$comb_eta), trtgroup = rep(c("trt","cnt"), each=nrow(res$comb_eta)/2)) %>%
  # gather("eta", "score", -id, -trtgroup) %>%
  ggplot() +
  geom_point(aes(x = pop.est1, y = est.eta1, color = trtgroup))

data.frame(res$comb_eta[],id = 1:nrow(res$comb_eta), trtgroup = rep(c("trt","cnt"), each=nrow(res$comb_eta)/2)) %>%
  # gather("eta", "score", -id, -trtgroup) %>%
  ggplot() +
  geom_point(aes(x = pop.est2, y = est.eta2, color = trtgroup))


data.frame(res$comb_eta[],id = 1:nrow(res$comb_eta), trtgroup = rep(c("trt","cnt"), each=nrow(res$comb_eta)/2)) %>%
  # gather("eta", "score", -id, -trtgroup) %>%
  ggplot() +
  geom_point(aes(x = pop.est1, y = est.eta1, color = trtgroup))

data.frame(res$comb_eta[],id = 1:nrow(res$comb_eta), trtgroup = rep(c("trt","cnt"), each=nrow(res$comb_eta)/2)) %>%
  # gather("eta", "score", -id, -trtgroup) %>%
  ggplot() +
  geom_point(aes(x = est.eta1, y = est.eta2, color = trtgroup))

