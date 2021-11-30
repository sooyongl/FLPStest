# rm(list= ls()); gc()
library(tidyverse); library(rstan);library(lavaan)

# IRT ---------------------------------------------------------------------
true_eta <- rnorm(500)
info <- FLPS:::parsForLVM(theta = true_eta, nsec  = 10, data_type = "2pl")
data_info <- FLPS:::generate.dich(info)

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

pop.diff <- data_info$lv.par$b
pop.disc <- data_info$lv.par$a

write_csv(data.frame(data_info$resp), "test/test_lv_model/mplus/irt.csv", col_names = F)

MplusAutomation::runModels("test/test_lv_model/mplus/irt_loading_fixed.inp")
MplusAutomation::runModels("test/test_lv_model/mplus/irt_meanvar_fixed.inp")

irt_loading_fixed <- MplusAutomation::readModels("test/test_lv_model/mplus/irt_loading_fixed.out")
irt_meanvar_fixed <- MplusAutomation::readModels("test/test_lv_model/mplus/irt_meanvar_fixed.out")

mplus.lam1 <- list(
  lambda = irt_loading_fixed$parameters$unstandardized %>% filter(str_detect(paramHeader, "F.BY")) %>% pull(est),
  tau = irt_loading_fixed$parameters$unstandardized %>% filter(str_detect(paramHeader, "Thresholds")) %>% pull(est),
  diff = irt_loading_fixed$parameters$irt.parameterization %>% filter(str_detect(paramHeader, "Diffi")) %>% pull(est),
  psi = irt_loading_fixed$parameters$unstandardized %>% filter(str_detect(paramHeader, "Varia")) %>% pull(est),
  alpha = irt_loading_fixed$parameters$unstandardized %>% filter(str_detect(paramHeader, "Means")) %>% pull(est)
)

mplus.var1 <- list(
  lambda = irt_meanvar_fixed$parameters$unstandardized %>% filter(str_detect(paramHeader, "F.BY")) %>% pull(est),
  tau = irt_meanvar_fixed$parameters$unstandardized %>% filter(str_detect(paramHeader, "Thresholds")) %>% pull(est),
  diff = irt_meanvar_fixed$parameters$irt.parameterization %>% filter(str_detect(paramHeader, "Diffi")) %>% pull(est),
  psi = irt_meanvar_fixed$parameters$unstandardized %>% filter(str_detect(paramHeader, "Varia")) %>% pull(est),
  alpha = irt_meanvar_fixed$parameters$unstandardized %>% filter(str_detect(paramHeader, "Means")) %>% pull(est)
)


stan_model <- paste(read_lines("test/test_lv_model/stan/IRT.stan"),
                    collapse = "\n")
cat(stan_model)

twopl.fit <- rstan::stan(
  model_code = stan_model,
  data = dt,
  iter = 4000,
  cores = 1,
  chains = 3
)


# cleaning ----------------------------------------------------------------
twopl.fit <- as.data.frame(twopl.fit)

stan.disc1 <- colMeans(twopl.fit[str_detect(names(twopl.fit), "disc1")])
stan.diff1 <- colMeans(twopl.fit[str_detect(names(twopl.fit), "diff1")])

data.frame(pop.disc = pop.disc, mplus.lam1$lambda, mplus.var1$lambda, stan.disc1)

data.frame(
  pop.diff,
  mplus.lam1.diff = mplus.lam1$diff,
  mplus.var1.diff = mplus.var1$diff,
  stan.diff1 = -stan.diff1 / stan.disc1
  )


eta <- cbind(est_eta = colMeans(twopl.fit[str_detect(names(twopl.fit), "eta")]), pop_eta = true_eta)

c(pop_mean = mean(eta[,2]), mplus_mean = mplus.lam1$alpha, stan.mean = mean(eta[,1]))

c(pop_var = var(eta[,2]), mplus_var = mplus.lam1$psi, stan.var = var(eta[,1]))
plot(eta[,"est_eta"],eta[,"pop_eta"])


# polytomous --------------------------------------------------------------

