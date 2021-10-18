# rm(list = ls())
for(i in fs::dir_ls("R", regexp = "r$")) source(i)

memory.limit(50000)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
getOption("mc.cores")

# 4000 /// 1000
# 10/25 // 40//100

conditions <- data.frame(
  samplesize = c(1000, 1000, 4000, 4000),
  lambda = c(10, 40, 10, 40),
  nsec = c(25, 100, 25, 100)
)

for(j in 1:nrow(conditions)) {
  
  N      <- conditions$samplesize[j]
  lambda <- conditions$lambda[j]
  nsec   <- conditions$nsec[j]
  
  # data generation ---------------------------------------------------------
  parsFromMod <- list(
    N = N, # sample size
    R2Y = 0.2, ## from app
    omega = 0.2,
    tau0 = 0.13, ## from paper
    tau1 = -0.06, ## from paper "a difference of one IQR in etaT was associated with a reduction of 0.083 in the effect size" 0.083/1.35~~ 0.06
    lambda = lambda, ## from data used in model
    R2eta = 0.5, ## from app
    nsec = nsec, ## from data used in model
    lvmodel = "rasch" # tag for latent variable model
  )
  
  for(i in 1:20) {
    
    set.seed(i+N+nsec)
    
    sdat <- do.call("makeDat", parsFromMod)
    
    # N_lambda_nsec_
    filename <- paste(parsFromMod$N, parsFromMod$lambda, parsFromMod$nsec, "rstan_fit", i, ".rds", sep = "_")
    
    # run stan --------------------------------------------------------------
    fit <- rstan::stan("R/psRasch.stan",data=sdat)
    
    saveRDS(list(fit = fit, sdat = sdat), file.path("results_temp", filename))
    
    print(i)
  }
  
  print(j)
}

# system("shutdown -f -t 300")

# output ------------------------------------------------------------------
files <- fs::dir_ls("results")

res1 <- lapply(files, function(i) { # i <- files[1]
  fit <- readRDS(i)
  
  pop_data <- fit$sdat
  fit <- fit$fit
  
  # Bayesian fit -----------------------
  # ESS / Rhat / geweke / heidel / raftery
  sims <- matrix(unlist(lapply(fit@sim$samples, "[[", "b1")), ncol = fit@sim$chains)
  r_h <- round(Rhat(sims), 3)
  
  mcmc_object <- As.mcmc.list(fit)
  # # effectiveSize(mcmc_object)
  gew <- geweke.diag(mcmc_object,frac1=.1,frac2=.5)
  if(all(abs(unlist(lapply(lapply(gew, "[[", "z"), "[[", "b1"))) < 1.96)){
    gew <- "converge"
    } else {
    gew <- "noncon"
  }
  
  hei <- heidel.diag(mcmc_object)
  if(all(unlist(lapply(hei, function(x) {x[][which(rownames(x[]) == "b1"), ][4]}))==1)){
    hei <- "converge"
  } else{
    hei <- "noncon"
  }
  gel <- gelman.diag(mcmc_object)
  gel <- gel$mpsrf
  
  # raf <- raftery.diag(mcmc_object)
  # raf
  
  model_fit <- list(list(rhat = r_h, geweke = gew, heidel = hei, gelman = gel))
  
  # Estimates -----------------------
  fit <- as.data.frame(fit)
  
  temp1 <- str_split(i, "/", simplify = T)[,2]
  temp2 <- str_split(temp1, "_", simplify = T)
  sample_size <- temp2[1,1]
  lambda <- temp2[1,2]
  nsec <- temp2[1,3]
  rep <- temp2[1,6]
  
  a1 <- mean(fit$a1)
  b0 <- mean(fit$b0)
  b1 <- mean(fit$b1)
  
  diff <- list(
    est = colMeans(fit[str_detect(names(fit), "secEff")]),
    pop = -pop_data$lv.par$b
    )
  
  df <- tibble(sample_size, lambda, nsec, rep, model_fit, b1, a1, diff)
  
  df
})

res2 <- bind_rows(res1)

saveRDS(res2, "report/combined_result.rds")


# -------------------------------------------------------------------------
res2 <- readRDS("report/combined_result.rds")

mean_b1 <- res2 %>% 
  group_by(sample_size, lambda, nsec) %>% 
  summarise(
    b1_m = mean(b1)
  )

library(ggpubr); library(ggrepel)
p1 <- res2 %>% 
  ggplot(aes(y = b1, x = sample_size, colour = nsec)) +
  geom_boxplot(
    size = 0,
    position = position_dodge(1),
  ) +
  geom_point(
    size = 3,
    alpha = 0.3,
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 1)
  ) +
  geom_hline(yintercept = -0.06) +
  
  geom_point(
    data = mean_b1,
    aes(y = b1_m, x = sample_size, colour = nsec),
    size = 3, 
    stroke = 1, 
    shape = 21,
    alpha = 1,
    position = position_jitterdodge(jitter.width = 0, dodge.width = 1)
  ) +
  geom_label(
    data = mean_b1,
    aes(y = b1_m, x = sample_size, colour = nsec, label = round(b1_m, 3)),
    position = position_jitterdodge(jitter.width = 0, dodge.width = 1)
  ) +
  theme_pubclean()
set_palette(p1, "jco")










# str(fit@model_pars)
# print(fit, pars = 'betaU')
# print(fit, pars = 'betaY')
# 
# print(fit, pars = 'b00')
# print(fit, pars = 'a1')
# 
# methods(print, class = class(fit))
# graphs ------------------------------------------------------------------- 
# stan_hist(fit, bins=40)
# 
# fit.ggmcmc <- ggs(fit)
# theme_set(theme_bw())
# 
# fit.ggmcmc %>% filter(Parameter == "studEff[1]") %>% ggs_traceplot(.)
# fit.ggmcmc %>% filter(Parameter == "studEff[1]") %>% ggs_density(.)
# fit.ggmcmc %>% filter(Parameter == "studEff[1]") %>% ggs_caterpillar(.)

