# rm(list = ls())
# for(i in fs::dir_ls("R", regexp = "r$")) {print(i);source(i)}; rm(i)
# source_funs <- ls()
#
# # memory.limit(50000)
# # rstan_options(auto_write = TRUE)
# # options(mc.cores = detectCores() - 4 )
# # getOption("mc.cores")
#
# # 4000 /// 1000
# # 10/25 // 40//100
#
# conditions <- data.frame(
#   samplesize = c(1000),
#   lambda = c(10),
#   nsec = c(20)
# )
#
# conditions <- crossing(conditions, rep = 1:20)
# conditions %>% print(n = 40)
#
# n_cores <- detectCores() - 4
# cl <- parallel::makeCluster(n_cores)
# doSNOW::registerDoSNOW(cl)
#
# pb <- txtProgressBar(max=nrow(conditions), style=3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress=progress)
#
# o <- foreach(j = 1:nrow(conditions),
#              .export = source_funs,
#              .packages = c("rstan"),
#              .options.snow = opts) %dopar% {
# # for(j in 1:nrow(conditions)) {
#   # j = 1
#   print(j)
#
#   REP      <- conditions$rep[j]
#
#   N      <- conditions$samplesize[j]
#   lambda <- conditions$lambda[j]
#   nsec   <- conditions$nsec[j]
#
#   # data generation ---------------------------------------------------------
#   parsFromMod <- list(
#     N = 100, # sample size
#     R2Y = 0.2, ## from app
#     omega = 0.2,
#     tau0 = 0.13, ## from paper
#     tau1 = -0.06, ## from paper "a difference of one IQR in etaT was associated with a reduction of 0.083 in the effect size" 0.083/1.35~~ 0.06
#     lambda = lambda, ## from data used in model
#     R2eta = 0.2, ## from app
#     nsec = nsec, ## from data used in model
#     lvmodel = "rasch" # tag for latent variable model
#   )
#
#
#   # set.seed(i+N+nsec)
#
#   sdat <- do.call("makeDat", parsFromMod)
#
#   # N_lambda_nsec_
#   filename <- paste(parsFromMod$N, parsFromMod$lambda, parsFromMod$nsec,
#                     "xeff", REP, ".rds", sep = "_")
#
#   # # run stan --------------------------------------------------------------
#   fit <- stan(
#     cores = 1,
#     "R/psRasch.stan",
#     data = sdat,
#     chains = 4,
#     iter = 4000,
#     warmup = 1000
#   )
#
#   saveRDS(list(fit = fit, sdat = sdat), file.path("results", filename))
#
#
#   rm(fit)
#   gc()
#
#   NULL
# }
# stopCluster(cl)
# # system("shutdown -f -t 300")
