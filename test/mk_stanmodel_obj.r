library(rstan)
stanfiles <- list.files("inst/stan/", pattern = "stan$",full.names = T)
stanfiles <- stanfiles[grep("univ",stanfiles)]
# stanfilename <- paste0("inst/stan/ps", toupper(ifelse(lvmodel=="2pl", "irt",
#                                                       lvmodel)),"_univ.stan")
for(i in 1:length(stanfiles)) {
  # i = 2

  stan_model <- paste(readLines(stanfiles[i]), collapse = "\n")

  stanmodel_obj <- stan_model(model_code = stan_model,
                              model_name = gsub("inst/stan/", "", stanfiles[i]))

  saveRDS(stanmodel_obj, gsub("\\.stan", "\\.rds", stanfiles[i] ))
  print(i)
}

