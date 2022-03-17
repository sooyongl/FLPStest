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
  linear  = T,
  ydist   = 'n',
  lambda  = 0.6,
  nsec    = 20,
  nfac    = 1,
  lvmodel ="grm"
)

sdat <- do.call("makeDat", sim_condition)


# saveRDS("test/test_sdata_0.rds")
# sdat <- readRDS("test/test_sdata_0.rds")

# check LV part -----------------------------------------
check_lv(
  dat = sdat$lv.resp,
  covdata = NULL,
  # covdata = data.frame(sdat$x),,
  lvmodel = sdat$lvmodel,
  nfac = sdat$nfac,
  IRTpars = ifelse(sdat$lvmodel=="2pl", T, F)
)
# item parameter
sdat$lvinfo$ipar

# check model part --------------------------------------
check_flps(sdat$stan_dt)

# run flps ----------------------------------------------
fit <- rstan::stan(
  # file = "inst/stan/psIRT_multi.stan",
  file = "inst/stan/example_model.stan",
  # file = "inst/stan/psGRM_multi.stan",
  data = sdat$stan_dt,
  # iter = 6000,
  # warmup = 2000,
  chains = 1,
  open_progress = F
)
o <- list(fit=fit, sdat=sdat)
saveRDS(o, "test/test_model_gpcm.rds")


# cleaning ----------------------------------------------
# o <- readRDS("test/test_model_0.rds")
rds_list <- fs::dir_ls("results")
res_list <- vector("list", length(rds_list))
for(i in 1:length(rds_list)) {

  condition <- str_split(rds_list[i], "/",simplify = T)[2]
  condition <- str_split(condition, ".rds", simplify = T)[1]

  rds_file <- rds_list[i]

  o <- readRDS(rds_file)

  fit  <- o$fit
  sdat <- o$sdat

  res <- clean_temp(fit = fit, sdat = sdat)

  flps_param <- data.frame(res$flps_param)

  flps_param$model = condition
  flps_param$param_name = rownames(flps_param)

  res_list[[i]] <- flps_param
}
res_list <- do.call("rbind", res_list)


saveRDS(res_list, "report/0217_res_list.rds")







# data.frame(res$comb_lambda) %>%
#   set_names(paste0(rep(c("pop.a", "est.a"), each = ncol(res$comb_lambda)), 1:ncol(res$comb_lambda))) %>%
#   tibble()
#
#
# data.frame(res$comb_tau[,c(1,3)]) %>%
#   set_names(c("pop.b","est.b")) %>%
#   tibble()
#
# res$flps_param
#
# colnames(res$comb_eta) <- paste0(rep(c("pop.eta", "est.eta"), each = (ncol(res$comb_eta)/2)), 1:(ncol(res$comb_eta)/2))
# round(cor(res$comb_eta),3)
# mean(res$comb_eta[,1] - res$comb_eta[,3])
# mean(res$comb_eta[,2] - res$comb_eta[,4])
#
# mean((res$comb_eta[,1] - res$comb_eta[,3])^2)
# mean((res$comb_eta[,2] - res$comb_eta[,4])^2)
#
# data.frame(res$comb_eta[],id = 1:nrow(res$comb_eta), trtgroup = rep(c("trt","cnt"), each=nrow(res$comb_eta)/2)) %>%
#   # gather("eta", "score", -id, -trtgroup) %>%
#   ggplot() +
#   geom_point(aes(x = pop.eta1, y = est.eta1, color = trtgroup))
#
# data.frame(res$comb_eta[],id = 1:nrow(res$comb_eta), trtgroup = rep(c("trt","cnt"), each=nrow(res$comb_eta)/2)) %>%
#   # gather("eta", "score", -id, -trtgroup) %>%
#   ggplot() +
#   geom_point(aes(x = pop.eta2, y = est.eta2, color = trtgroup))
#
#
# data.frame(res$comb_eta[],id = 1:nrow(res$comb_eta), trtgroup = rep(c("trt","cnt"), each=nrow(res$comb_eta)/2)) %>%
#   # gather("eta", "score", -id, -trtgroup) %>%
#   ggplot() +
#   geom_point(aes(x = pop.eta1, y = pop.eta2, color = trtgroup))
#
# data.frame(res$comb_eta[],id = 1:nrow(res$comb_eta), trtgroup = rep(c("trt","cnt"), each=nrow(res$comb_eta)/2)) %>%
#   # gather("eta", "score", -id, -trtgroup) %>%
#   ggplot() +
#   geom_point(aes(x = est.eta1, y = est.eta2, color = trtgroup))
#
