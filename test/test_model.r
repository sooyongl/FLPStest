rm(list = ls())
library(rstan); library(tidyverse)
for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)
source_funs <- ls()

memory.limit(50000)
rstan_options(auto_write = TRUE)
options(mc.cores = 8)

file_list <- c(
  "inst/stan/psIRT_scaled.stan",
  "inst/stan/psGPCM_scaled.stan",
  "inst/stan/psSEM_scaled.stan",
  "inst/stan/psLCA.stan"
)
lvmodel_list <- c("2PL", "GPCM", "sem","lca")
#
# model ---------------------------------------------------------
stan_file_name <- file_list[4]
lvmodel <- lvmodel_list[4]

# for(l in 0.6) { #seq(0.2,1, 0.2)) {
  for(i in 1:1) {
    l <- 0.6 #seq(0.2,1, 0.2)[3]
    # i <- 1
    # -------------------------------------------------------------------------
    parsFromMod <- list(
      N = 500, # sample size
      R2Y = 0.2,
      omega = 0.2,  # a1
      tau0 = 0.4,  # b0
      tau1 = -0.2, # b1
      # lambda = 10,
      lambda = 0.6,
      R2eta = 0.2,
      nsec = 20,
      lvmodel = lvmodel # tag for latent variable model
    )
    sdat <- do.call("makeDat", parsFromMod)

    # data <- data.frame(Y=sdat$Y,Z=sdat$Z,sdat$X,eta=sdat$true_eta)
    # lm(Y ~ Z + x1 + x2 + Z*eta, data = data)

    # glm(class ~ x1 + x2, data.frame(class = sdat$lv.rep$class-1, sdat$X),
    #     family = "binomial")

    # sdat <- do.call("makeInpData", parsFromMod)
    #
    # lv_data_mat <- sdat$inp_data[, paste0("X",1:20)]
    # sort(apply(lv_data_mat, 2, function(x) {sum(!is.na(x))}), decreasing = T)
    # apply(lv_data_mat, 1, function(x) {sum(!is.na(x))})

    # sdat1 <- makeFLPSdata(inp_data = sdat$inp_data,
    #              outcome = "Y",
    #              group = "Z",
    #              covariate = c("x1","x2"),
    #              lv_model = paste0("F=~", paste(paste0("X",1:20),collapse = "+")),
    #              lv_type = lvmodel)
    #
    # sdat <- sdat1@stan_data
    sdat$nclass <- 2

    fit <- stan(stan_file_name,data=sdat,iter=5000,warmup=2000,chains=1)

    # stan_file_noinfo <- "inst/stan/psIRT_scaled_noinfo.stan"
    # fit <- stan(stan_file_noinfo,data=sdat,iter=10000,warmup=2000,chains=4)

    # print(fit, c("b1"))
    # print(fit, c("lambda[2]","lambda[3]","lambda[4]","lambda[5]"))
    #
    # stan_vb <- stan_model(file = stan_file_noinfo)
    # vb_fit <-
    #   vb(
    #     stan_vb,
    #     data = sdat,
    #     iter = 15000,
    #     elbo_samples = 1000,
    #     algorithm = c("meanfield"),
    #     # imporance_resampling = T,
    #     output_samples = 10000,
    #     tol_rel_obj = 0.00001
    #   )
    # print(vb_fit, c("b1"))

    res <- list(fit = fit, sdat = sdat)
    saveRDS(res, paste0("results/res_", l,"_", parsFromMod$lvmodel,i ,".rds"))

    print(i)
    gc()
  }
# }

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
res_list <- fs::dir_ls("results", regexp = "rds$")

res_list <- res_list[str_detect(res_list, "2PL")]
res_list <- res_list[str_detect(res_list, "_info[1-9]")]

param_list <- vector("list", length(res_list))
for(i in 1:length(res_list)) {
  model_type <- str_split(res_list[i], "_", simplify = T)[2]
  scaley <- str_split(res_list[i], "_", simplify = T)[3]

  res <- readRDS(res_list[i])

  fit <- res$fit
  sdat <- res$sdat

  # item param
  df.fit <- as.data.frame(fit)
  df.fit$chain <- rep(c(1:4), each = 8000)
  df.fit <- df.fit %>% group_by(chain)
  #
  # df.fit %>%
  #   select(matches("^nu|^p|^b00|^b01|betaY|betaU")) %>%
  #   summarise_all(mean)


  lambda <- df.fit %>%
    select(chain, matches("^lambda\\[")) %>%
    summarise_all(mean)

  tau <- df.fit %>%
    select(chain, matches("^tau\\[")) %>%
    summarise_all(mean)

  # lambda <- colMeans(df.fit[str_detect(names(df.fit), "alpha\\[")])
  # tau <- colMeans(df.fit[str_detect(names(df.fit), "beta\\[")])
  # tau <- matrix(tau, nrow = length(lambda))

  if(suppressWarnings(str_detect(evaluate::evaluate("sdat$lv.par[,2]")[[2]], "Error"))) {
    sdat$lv.par <- cbind(sdat$lv.par[,1], 0)
  }

  comb_lambda <- rbind(c(0, sdat$lv.par[,1]),lambda)
  comb_tau <- rbind(c(0, sdat$lv.par[,2]), tau)


  eta <- df.fit %>%
    select(chain, matches("^eta")) %>%
    summarise_all(mean)

  comb_eta <- rbind(c(0, sdat$true_eta), eta)

  # plot(unlist(comb_eta[1,]), unlist(comb_eta[2,]))
  # plot(unlist(comb_eta[1,]), unlist(comb_eta[3,]))
  # plot(unlist(comb_eta[1,]), unlist(comb_eta[4,]))
  # plot(unlist(comb_eta[1,]), unlist(comb_eta[5,]))

#   eta <- data.frame(
#     est_eta = colMeans(df.fit[str_detect(names(df.fit), "^eta")]),
#     pop_eta = sdat$true_eta)

  # mu.eta <- c(pop_mean = mean(eta[,2]), stan.mean = mean(eta[,1]), mueta = colMeans(df.fit[str_detect(names(df.fit), "muEta")]))

  # plot(eta[,"est_eta"],eta[,"pop_eta"])
  # cor(eta[,"est_eta"],eta[,"pop_eta"])
  # main effect
  true_data <- data.frame(Y = sdat$Y, X1 = sdat$X[,1], X2 = sdat$X[,2], eta = sdat$true_eta, Z = sdat$Z)
  true_param <- lm(Y ~ Z + eta + eta*Z + X1 + X2, data = true_data)
  true_param <- coefficients(true_param)

  true_param1 <- lm(eta ~ X1 + X2, data = true_data)
  true_param1 <- coefficients(true_param1)

  true_param <- c(0, true_param, true_param1[2:3])

  names(true_param) <- c("chain","b00", "b0", "a1", "by1","by2", "b1", "bu1","bu2")
  true_param <- true_param[c("chain", "b00", "b0", "b1", "a1", "by1","by2", "bu1","bu2")]

  est_param <- df.fit %>%
    select(chain, matches("^b00|^b0|b1|a1|betaY|betaU")) %>%
    summarise_all(mean) %>%
    set_names(c("chain","bu1","bu2","by1","by2","b00","a1","b0","b1")) %>%
    select(chain, b00, b0, b1, a1, by1, by2, bu1, bu2)

  # est_param <- data.frame(
  #   b00  = colMeans(df.fit[str_detect(names(df.fit), "b00$")]),
  #   b0  = colMeans(df.fit[str_detect(names(df.fit), "b0$")]),
  #   b1  = colMeans(df.fit[str_detect(names(df.fit), "b1$")]),
  #   a1  = colMeans(df.fit[str_detect(names(df.fit), "a1$")]),
  #   by1  = colMeans(df.fit[str_detect(names(df.fit), "betaY")])[1],
  #   by2  = colMeans(df.fit[str_detect(names(df.fit), "betaY")])[2],
  #   bu1  = colMeans(df.fit[str_detect(names(df.fit), "betaU")])[1],
  #   bu2  = colMeans(df.fit[str_detect(names(df.fit), "betaU")])[2]
  # )

  flps_param <- rbind(true = true_param, est = est_param)

  # param_list[[i]] <- list(model_type = model_type, lv_param = lv_param, eta_param = eta, mu.eta = mu.eta, flps_param = flps_param)

  eta_bias <-
    comb_eta %>%
    gather("etan", "eta_est", -chain) %>%
    mutate(chain = rep(0:4, 500)) %>%
    arrange(chain) %>%
    mutate(
      true_eta = rep(abs(sdat$true_eta), 5),
      eta_est = abs(eta_est)
      ) %>%
    group_by(chain) %>%
    summarise(
      bias = mean(eta_est - true_eta),
      rmse = sqrt(mean((eta_est - true_eta)^2)),
    )

  param_list[[i]] <-
    list(model_type = paste0(model_type, "_", scaley),
         comb_lambda = comb_lambda,
         comb_tau = comb_tau,
         eta_param = comb_eta,
         flps_param = flps_param,
         eta_bias = eta_bias)

  print(i)
  gc()
}
saveRDS(param_list, "report/rds/irt_info_1214.rds")
# param_list[[11]]
#
# param_list.2PL <- param_list
#
#
# param_list.sem <- param_list
# param_list.gpcm <- param_list
#
#
# res_combined_1130 <- list(param_list.sem = param_list.sem,
#                           param_list.2PL = param_list.2PL,
#                           param_list.gpcm = param_list.gpcm)
#
# saveRDS(res_combined_1130, "report/rds/res_combined_complete_1130.rds")

# -------------------------------------------------------------------------
param_info_list <- readRDS("report/rds/irt_info_1214.rds")
param_noinfo_list <- readRDS("report/rds/irt_noinfo_1214.rds")

ns <- 13
ys <- 14


lapply(param_list, "[[", "comb_lambda")
lapply(param_list, "[[", "eta_param")
lapply(param_list, "[[", "flps_param") %>%
  bind_rows(.id = "rep") %>%
  filter(chain != 0) %>%
  ggplot() +
  geom_point(aes(x = as.factor(chain), y = b1, colour = rep),
             size = 1.2) +
  geom_hline(yintercept = -0.2, size = 1.2)

a1 <- param_list[[1]]

plotlist <- mk_plot(a1)

plotlist$p_lambda
plotlist$p_tau
plotlist$p_eta
plotlist$p_eta_group
plotlist$t_param


mk_plot <- function(picked) {

  model_type <- picked$model_type

  p_lambda <- picked$comb_lambda %>%
    set_names(c("chain", paste0("fs",1:20))) %>%
    gather("name", "value", -chain) %>%
    ggplot() +
    geom_point(aes(name, value,
                   color = as.factor(chain))) +
    geom_line(
      aes(name, value,
          color = as.factor(chain),
          linetype = as.factor(chain),
          group = as.factor(chain)),
      size = 1.2) +
    labs(color = "Chain",
         x = "factor loading",
         y = "est",
         title = "Factor loading", subtitle = picked) +
    theme_bw(base_size = 16)

  p_tau <- picked$comb_tau %>%
    set_names(c("chain", paste0("int",1:20))) %>%
    gather("name", "value", -chain) %>%
    ggplot() +
    geom_point(aes(name, value,
                   color = as.factor(chain))) +
    geom_line(
      aes(name, value,
          color = as.factor(chain),
          linetype = as.factor(chain),
          group = as.factor(chain)),
      size = 1.2) +
    labs(color = "Chain",
         x = "intercept",
         y = "est",
         title = "Intercept (or difficulty)", subtitle = picked) +
    theme_bw(base_size = 16)

  eta_param <- picked$eta_param %>%
    gather("name", "value", -chain)

  pop_eta <- eta_param %>% filter(chain == 0)
  est_eta <- eta_param %>% filter(chain != 0) %>% arrange(chain)



  est_eta <- est_eta %>%
    mutate(
      group = rep(rep(c("trt","ctl"), each = 250), 4),
      pop_eta = rep(pop_eta$value,4))


  p_eta_group <- est_eta %>%
    ggplot() +
    geom_point(aes(pop_eta, value,
                   color = group),
               alpha = 0.5) +
    geom_smooth(
      aes(pop_eta, value,
          color = group),
      method='lm', formula= y~x,
      se = F
    ) +
    facet_wrap(chain ~ .) +
    labs(color = "Chain",
         y = "Est eta",
         title = "Correlation between true eta and est eta by group",
         subtitle = picked) +
    theme_bw(base_size = 16)

  p_eta <-est_eta %>%
    ggplot() +
    geom_point(aes(pop_eta, value,
                   color = as.factor(chain)),
               alpha = 0.5) +
    geom_smooth(
      aes(pop_eta, value,
          color = as.factor(chain)),
      method='lm', formula= y~x,
      se = F
    ) +
    facet_wrap(chain ~ .) +
    labs(color = "Chain",
         y = "Est eta",
         title = "Correlation between true eta and est eta",
         subtitle = picked) +
    theme_bw(base_size = 16)

  t_param <- picked$flps_param

  list(p_lambda = p_lambda,
       p_eta = p_eta,
       t_param = t_param,
       p_tau = p_tau,
       p_eta_group = p_eta_group)
}

