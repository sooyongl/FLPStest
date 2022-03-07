# rm(list = ls())
library(rstan)
library(foreach)
library(doParallel)
library(Rmpi)

for(i in list.files("R", full.names = T, pattern = "r$")) source(i);rm(i)
source_funs <- ls()

# core setting ------------------------------------------------------------
cl <- getMPIcluster()
doParallel::registerDoParallel(cl)

# n_cores <- detectCores() - 2; print(n_cores)
# cl <- parallel::makeCluster(n_cores)
# doSNOW::registerDoSNOW(cl)


rds_list <- list.files("results", full.names = T, pattern = ".rds")
res_list <- foreach(
    i = 1:length(rds_list),
    .packages = c("rstan"),
    .errorhandling = 'pass',
    .export = source_funs
  ) %dopar% {

  condition <- unlist(strsplit(rds_list[i], "/"))[2]
  condition <- unlist(strsplit(condition, ".rds"))[1]

  rds_file <- rds_list[i]

  o <- readRDS(rds_file)

  fit  <- o$fit
  sdat <- o$sdat

  res <- clean_temp(fit = fit, sdat = sdat)
  res$condition <- condition
  # res <- do.call("rbind", res)
  # res <- data.frame(condition=condition, par_name = rownames(res), res)

  # rownames(res) <- NULL

  # flps_param <- data.frame(res$flps_param)
  #
  # flps_param$model = condition
  # flps_param$param_name = rownames(flps_param)
  #
  # res_list[[i]] <- flps_param

  res
}
# res_list <- do.call("rbind", res_list)

saveRDS(res_list, "results/cleaned/0310_res_cleaned.rds")
