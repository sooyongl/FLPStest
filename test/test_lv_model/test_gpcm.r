# rm(list= ls()); gc()
library(tidyverse); library(rstan);library(lavaan)
for(i in fs::dir_ls("R", regexp = "r$")) source(i);

# generate data ------------------------------------------
nitem <- 10
nfac = 1
lvmodel = "grm"
ncat = 4

ipar <- genIRTpar(nitem, nfac, lvmodel, ncat)

lvmodel0 = ifelse(lvmodel == "grm", "graded", lvmodel)
response <- simData(a = as.matrix(ipar$a),
                    d = as.matrix(ipar[, grep("d",names(ipar))]),
                    guess = as.vector(0),
                    N = N,
                    theta = as.matrix(rnorm(5000)),
                    itemtype = lvmodel0)
response <- data.frame(response)
# check LV part -----------------------------------------
res_mirt <- check_lv(
  dat = response,
  covdata = NULL,
  # covdata = data.frame(sdat$x),
  lvmodel = lvmodel,
  nfac = nfac,
  IRTpars = ifelse(lvmodel=="gpcm", T, F)
)

coef(res_mirt$mirt.fit, simplify =T, IRTpars = T)$items
coef(res_mirt$mirt.fit, simplify =T, IRTpars = F)$items
ipar

lapply(1:4, function(x) {
  mean(res_mirt$items.est[, x] -ipar[, x])
})




# write_csv(data.frame(response), "test/test_lv_model/mplus/gpcm.csv",
#           col_names = F)
#
# MplusAutomation::runModels("test/test_lv_model/mplus/gpcm_loading_fixed.inp")
# MplusAutomation::runModels("test/test_lv_model/mplus/gpcm_meanvar_fixed.inp")
#
# irt_loading_fixed <- MplusAutomation::readModels("test/test_lv_model/mplus/gpcm_loading_fixed.out")
#
# irt_loading_fixed$parameters$unstandardized %>% filter(str_detect(paramHeader, "Steps")) %>% pull(est) %>%
#   matrix(., 10, 3, byrow = T) %>%
#   cbind(irt_loading_fixed$parameters$unstandardized %>% filter(str_detect(paramHeader, "F.BY")) %>% pull(est), .)
# ipar
#
# irt_loading_fixed$parameters$irt.parameterization
# ipar
# mplus.lam1 <- list(
#   lambda = irt_loading_fixed$parameters$unstandardized %>% filter(str_detect(paramHeader, "F.BY")) %>% pull(est),
#   tau = irt_loading_fixed$parameters$unstandardized %>% filter(str_detect(paramHeader, "Steps")) %>% pull(est),
#   psi = irt_loading_fixed$parameters$unstandardized %>% filter(str_detect(paramHeader, "Varia")) %>% pull(est)
# )
#
#
# stan_model <- paste(read_lines("test/test_lv_model/stan/GPCM.stan"),
#                     collapse = "\n")
# cat(stan_model)
#
# gpcm.fit <- rstan::stan(
#   model_code = stan_model,
#   data = dt,
#   iter = 4000,
#   cores = 1,
#   chains = 1
# )
#
# # cleaning ----------------------------------------------------------------
# gpcm.fit <- as.data.frame(gpcm.fit)
#
# stan.diff <- colMeans(gpcm.fit[str_detect(names(gpcm.fit), "beta_1")])
# stan.diff <- matrix(stan.diff, nsec, max_k-1)
#
# stan.disc <- colMeans(gpcm.fit[str_detect(names(gpcm.fit), "alpha_1")])
#
# data.frame(pop.disc = pop.disc,
#            mplus.lam1$lambda,
#            mplus.var1$lambda, stan.disc1)
#
# data.frame(
#   pop.diff,
#   mplus.lam1.diff = mplus.lam1$diff,
#   mplus.var1.diff = mplus.var1$diff,
#   stan.diff1 = stan.diff1
# )
#
#
# eta <- cbind(est_eta = colMeans(twopl.fit[str_detect(names(twopl.fit), "eta")]), pop_eta = true_eta)
#
# c(pop_mean = mean(eta[,2]), mplus_mean = mplus.lam1$alpha, stan.mean = mean(eta[,1]))
#
# c(pop_var = var(eta[,2]), mplus_var = mplus.lam1$psi, stan.var = var(eta[,1]))
# plot(eta[,"est_eta"],eta[,"pop_eta"])
#
#
#
#
