# rm(list = ls())
for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)

# output ------------------------------------------------------------------
files <- fs::dir_ls("results", regexp = "rds$")

# n_cores <- detectCores() - 4
# cl <- parallel::makeCluster(n_cores)
# doSNOW::registerDoSNOW(cl)
# 
# pb <- txtProgressBar(max=length(files), style=3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress=progress)

res1 <- vector("list", length(files))
# X <- bigstatsr::FBM(5, 5)
# res1 <-
#   foreach(i = 1:length(files),
#           .export = c("str_split","str_detect","tibble"),
#           .packages = c("rstan","coda", "ggplot2","bayesplot","purrr","dplyr"),
#           .options.snow = opts) %dopar%
for(i in 1:length(files))
{
  # i = 1
  print(i)
  
  fit <- readRDS(files[i])
  
  # print(object.size(fit), units="Mb")
  
  pop_data <- fit$sdat
  fit <- fit$fit
  
  # Bayesian fit -----------------------
  # ESS / Rhat / geweke / heidel / raftery
  sims <- matrix(unlist(lapply(fit@sim$samples, "[[", "b1")), ncol = fit@sim$chains)
  
  r_h <- round(Rhat(sims), 3)
  
  mcmc_object <- As.mcmc.list(fit, pars = "b1")

  gew <- geweke.diag(mcmc_object,frac1=.1,frac2=.5); # gew[[1]]$z
  if(all(abs(unlist(lapply(lapply(gew, "[[", "z"), "[[", "b1"))) < 1.96)){
    gew <- "converge"
  } else {
    gew <- "noncon"
  }
  
  hei <- heidel.diag(mcmc_object); # hei[[1]][]
  temp1 <- all(unlist(lapply(hei, function(x) {x[][which(rownames(x[]) == "b1"), ][4]}))==1) == 1
  if(!is.na(temp1) & temp1){
    hei <- "converge"
  } else{
    hei <- "noncon"
  }
  
  # gel <- gelman.diag(mcmc_object)
  # gel <- try(gel$mpsrf)
  # 
  # if(!is(gel, 'try-error')) {
  #   gel <- -9999
  # }
  
  # raf <- raftery.diag(mcmc_object)
  # raf
  
  model_fit <- list(list(rhat = r_h, geweke = gew, heidel = hei))
  
  # Estimates -----------------------
  fit <- as.data.frame(fit)
  
  temp1 <- str_split(files[i], "/", simplify = T)[,2]
  temp2 <- str_split(temp1, "_", simplify = T)
  sample_size <- temp2[1,1]
  lambda <- temp2[1,2]
  nsec <- temp2[1,3]
  niter <- temp2[1,4]
  rep <- temp2[1,5]
  
  a1 <- mean(fit$a1)
  # b0 <- mean(fit$b0)
  b1 <- mean(fit$b1)
  
  # b.x1 <- mean(fit$`betaY[1]`)
  # b.x2 <- mean(fit$`betaY[2]`)
  
  diff <- list(
    list(
      est = colMeans(fit[str_detect(names(fit), "secEff")]),
      pop = -pop_data$lv.par$b
    )
  )
  
  df <- tibble(temp1, sample_size, lambda, nsec, niter, rep, model_fit, b1, a1, diff)
  
  res1[[i]] <- df
  
  rm(fit, hei, gew)
  rm(mcmc_object)
  rm(o)
  
  gc()
  
}
# stopCluster(cl)

res1 <- do.call('rbind', res1)
saveRDS(res1, "report/combined_result_1028.rds")
