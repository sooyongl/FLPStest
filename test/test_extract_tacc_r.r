# rm(list = ls())
library(rstan)
library(foreach)
library(doParallel)
library(doSNOW)
library(Rmpi)

for(i in list.files("R", full.names = T, pattern = "r$")) source(i);rm(i)
source_funs <- ls()

# core setting ------------------------------------------------------------
cl <- getMPIcluster()
doParallel::registerDoParallel(cl)

rds_name <- list.files("results", full.names = T, pattern = "rds")

temp_rds_name <- list.files("results", full.names = F, pattern = "rds")
cleaned_name <- list.files("results/cleaned", full.names = F, pattern = "cleaned")
cleaned_name <- gsub("cleaned_","",cleaned_name)

rds_name <- rds_name[!temp_rds_name %in% cleaned_name]
# rds_name <- str_subset(rds_name, "rasch")
# rds_name
# length(rds_name)

res_list <- foreach(
  irds = 1:length(rds_name),
  # .packages = c("rstan","dplyr","stringr","purrr","magrittr","tidyr"),
  .packages = c("rstan"),
  .errorhandling = 'pass',
  .export = source_funs
  # ) %do% {
) %dopar% {
  # irds <- 1
  #
  # rds_name <- "results/rasch_1000_100_0.2_0.5_TRUE_n_39.rds"
  # rds_name <- "results/2pl_1000_100_0.2_0.5_TRUE_n_10.rds"
  # rds_name <- "results/gpcm_1000_200_0.2_0.5_TRUE_n_82.rds"
  # rds_name <- "results/grm_1000_50_0.2_0.5_TRUE_n_78.rds"

  print(irds)

  condition <- strsplit(rds_name[irds], "/", fixed=T)[[1]]
  condition <- condition[length(condition)]
  condition <- strsplit(condition, ".rds", fixed=T)[[1]][1]

  print(condition)

  # condition <- str_split(rds_name[irds], "/",simplify = T)
  # condition <- condition[length(condition)]
  # condition <- str_split(condition, ".rds", simplify = T)[1]

  res <- readRDS(rds_name[irds])
  fit <- res$fit
  sdat <- res$sdat

  stan_summary000 <- summary(fit)

  stan_summary <- stan_summary000$summary
  stan_chains <- stan_summary000$c_summary


  stan_parname <- rownames(stan_summary)
  # tail(stan_parname, 100); length(stan_parname)
  N <- sdat$N
  stan_dt <- sdat$stan_dt
  nb <- max(sdat$grad) - min(sdat$grad)
  nfac <- sdat$nfac

  iter = 5000 - 2000 #fit@sim$iter - fit@sim$warmup
  n.chain = 2 # fit@sim$chains

  df.fit <- as.data.frame(fit)
  if(sum(grep("free",names(df.fit))) == 0) {
    selected <- names(df.fit)
  } else {
    selected <- names(df.fit)[!grepl("free",names(df.fit))]
  }

  # df.fit <- df.fit[selected]
  #
  # df.fit$chain <- rep(c(1:n.chain), each = iter)
  # df.fit <- df.fit %>% group_by(chain)

  rm(res)

  # population --------------------------------------------------------------
  a11 <- sdat$omega
  b0  <- sdat$tau0
  b11 <- sdat$tau1
  true_param <- c(a11, 0, b0, b11, -1, 0.5, 1.0, 0.5)
  names(true_param) <- c("a1","b00","b0", "b1","betaU[1]","betaU[2]","betaY[1]","betaY[2]")

  # if(grepl("rasch", condition)) {
  #   true_param <- true_param[c("betaU[1]","betaU[2]","betaY[1]","betaY[2]",
  #                "b00","a1","b0", "b1")]
  # }


  true_ipar <- sdat$lv.par
  true_lam <- true_ipar[,(1:nfac)]

  true_tau <- true_ipar[,(nfac+1):ncol(true_ipar)]
  true_tau <- true_tau[!grepl("g", names(true_tau))]
  if(sdat$lvmodel != "2pl") {
    true_tau <- -true_tau

    if(sdat$lvmodel == "rasch") {
      true_tau <- -true_tau
    }
  }

  if(!sdat$lvmodel %in% c("grm","gpcm")) {
    true_tau <- unlist(true_tau)
  } else {

    true_tau <- apply(true_tau, 1, function(x) x)
    true_tau <- as.vector(true_tau)
  }


  true_eta  <- sdat$theta

  # estimates ---------------------------------------------------------------
  # selected <- names(df.fit)[grep("^chain|^eta",names(df.fit))]
  # eta_raw <- df.fit[selected]
  #
  #
  # eta_df <- eta_raw
  # eta_df <- lapply(1:n.chain, function(i) {
  #   apply(eta_df[eta_df$chain == i, ], 2, mean)
  # })
  # eta_df <- do.call("rbind", eta_df)
  # eta_df <- data.frame(eta_df)
  # selected <- names(eta_df)[!grepl("chain",names(eta_df))]
  # eta_df <- eta_df[selected]
  # eta_df <- t(eta_df)
  # eta_df <- data.frame(eta_df)
  # eta_df <- setNames(eta_df, paste0("est_eta", 1:n.chain))
  # eta_df$est_mean <- rowMeans(eta_df[,1:n.chain])
  # eta_df$par_name <- rownames(eta_df)
  # eta_df$true_eta <- true_eta
  #
  # eta_df <- eta_df[, c("par_name","true_eta", "est_eta1","est_eta2", "est_mean")]
  #
  # selected <- c("chain", names(df.fit)[grep("^lambda",names(df.fit))])
  # lambda_raw <- df.fit[selected]
  #
  # lambda_df <- lambda_raw
  # if(sdat$lvmodel == "rasch"){
  #   lambda_df$lambda <- 1
  # } else {
  #   lambda_df <- lambda_df
  # }
  # lambda_df <- lapply(1:n.chain, function(i) {
  #   apply(lambda_df[lambda_df$chain == i, ], 2, mean)
  # })
  # lambda_df <- do.call("rbind", lambda_df)
  # lambda_df <- data.frame(lambda_df)
  #
  # selected <- names(lambda_df)[!grepl("chain",names(lambda_df))]
  # lambda_df <- lambda_df[selected]
  # lambda_df <- t(lambda_df)
  # lambda_df <- data.frame(lambda_df)
  # lambda_df <- setNames(lambda_df, paste0("est_lam", 1:n.chain))
  # lambda_df$est_mean <- rowMeans(lambda_df[,1:n.chain])
  # lambda_df$par_name <- rownames(lambda_df)
  #
  # if(sdat$lvmodel == "rasch") {
  #   lambda_df$true_lam <- 1
  # }else {
  #   lambda_df$true_lam <- true_lam
  # }
  #
  # lambda_df <- lambda_df[, c("par_name","true_lam", "est_lam1","est_lam2", "est_mean")]
  #
  #
  # selected <- c("chain", names(df.fit)[grep("^tau",names(df.fit))])
  # tau_raw <- df.fit[selected]
  # tau_df <- tau_raw
  # tau_df <- lapply(1:n.chain, function(i) {
  #   apply(tau_df[tau_df$chain == i, ], 2, mean)
  # })
  #
  #
  #
  # tau_df <- do.call("rbind", tau_df)
  # tau_df <- data.frame(tau_df)
  # selected <- names(tau_df)[!grepl("chain",names(tau_df))]
  # tau_df <- tau_df[selected]
  #
  # tau_df <- t(tau_df)
  # tau_df <- data.frame(tau_df)
  #
  # tau_df <- setNames(tau_df, paste0("est_tau", 1:n.chain))
  # tau_df$est_mean <- rowMeans(tau_df[,1:n.chain])
  # tau_df$par_name <- rownames(tau_df)
  # tau_df$true_tau <- true_tau
  #
  # tau_df <- tau_df[, c("par_name", "true_tau", "est_tau1","est_tau2", "est_mean")]
  #
  #
  # selected <- c("chain", names(df.fit)[grep("^beta|^b|^a",names(df.fit))])
  # struct_raw <- df.fit[selected]
  #
  # struct_df <- struct_raw
  # struct_df <- lapply(1:n.chain, function(i) {
  #   apply(struct_df[struct_df$chain == i, ], 2, mean)
  # })
  # struct_df <- do.call("rbind", struct_df)
  # struct_df <- data.frame(struct_df)
  #
  # selected <- names(struct_df)[!grepl("chain",names(struct_df))]
  # struct_df <- struct_df[selected]
  # struct_df <- t(struct_df)
  # struct_df <- data.frame(struct_df)
  #
  # struct_df <- setNames(struct_df, paste0("est_struc", 1:n.chain))
  # struct_df$est_mean <- rowMeans(struct_df[,1:n.chain])
  # struct_df$par_name <- names(true_param)
  # struct_df$true_struc <- true_param
  #
  # struct_df <- struct_df[, c("par_name","true_struc", "est_struc1","est_struc2", "est_mean")]
  #
  # o <- list(
  #   condition = condition,
  #   # times = c(startt, rstanend),
  #   eta_raw = eta_raw,
  #   lambda_raw = eta_raw,
  #   tau_raw = eta_raw,
  #   struct_raw = struct_raw,
  #   eta_df = eta_df,
  #   lambda_df = lambda_df,
  #   tau_df = tau_df,
  #   struct_df = struct_df
  # )
  #
  # saveRDS(o, paste0("results/cleaned/extracted_",condition ,".rds"))
  #
  # if(dim(o$eta_df)[2] == 6) {
  #   names(o$eta_df) <- c("par_name","true_param","est_chain1","est_chain2","est_chain3","est_mean")
  #   names(o$lambda_df) <- c("par_name","true_param","est_chain1","est_chain2","est_chain3","est_mean")
  #   names(o$tau_df) <- c("par_name","true_param","est_chain1","est_chain2","est_chain3","est_mean")
  #   names(o$struct_df) <- c("par_name","true_param","est_chain1","est_chain2","est_chain3","est_mean")
  # }
  # if(dim(o$eta_df)[2] == 5) {
  #   names(o$eta_df) <- c("par_name","true_param","est_chain1","est_chain2","est_mean")
  #   names(o$lambda_df) <- c("par_name","true_param","est_chain1","est_chain2","est_mean")
  #   names(o$tau_df) <- c("par_name","true_param","est_chain1","est_chain2","est_mean")
  #   names(o$struct_df) <- c("par_name","true_param","est_chain1","est_chain2","est_mean")
  # }


  # eta_df <- do.call("rbind", list(o$eta_df,o$lambda_df,o$tau_df,o$struct_df))
  # eta_df$err <- eta_df$est_mean - eta_df$true_param
  # eta_df$cond <- o$condition
  # saveRDS(eta_df, paste0("results/cleaned/cleaned_",condition ,".rds"))


  if(is.null(stan_summary)) next

  if(grepl("rasch",condition)) {
    parmav <- c(true_param,true_eta,true_tau)
  } else {
    parmav <- c(true_param,true_eta,true_lam,true_tau)
  }
  str_param <- names(true_param)

  stan_parname <- c(str_param,
    rownames(stan_summary)[grep("eta\\[", rownames(stan_summary))],
    rownames(stan_summary)[grep("lambda\\[", rownames(stan_summary))],
    rownames(stan_summary)[grep("tau\\[", rownames(stan_summary))])

  str_summary <- stan_summary[str_param,]

  meas_summary <- stan_summary[
    c(
      # which(rownames(stan_summary) %in% names(true_param)),
      grep("eta\\[", rownames(stan_summary)),
      grep("lambda\\[", rownames(stan_summary)),
      grep("tau\\[", rownames(stan_summary))), ]

  stan_summary <- rbind(str_summary, meas_summary)

  stan_res <- data.frame(par_name = stan_parname,
                         true_parm = parmav,
                         stan_summary,
                         cond = condition)
  stan_res$err = stan_res$mean - stan_res$true_parm

  o <- list(stan_res = stan_res, stan_chains = stan_chains, condition = condition)


  saveRDS(o, paste0("results/cleaned/cleaned_",condition ,".rds"))

  NA
}
# stopCluster(cl)

# cleaned <- fs::dir_ls("F:/FLPS/results/cleaned", regexp = "cleaned_")
#
# combined <- foreach(i = 1:length(cleaned)) %do% {
#   a1 <- readRDS(cleaned[i])
#   a1
# } %>%
#   bind_rows()
#
# data_idx <- "0516"
# saveRDS(combined, paste0("results/cleaned/",data_idx,"_extracted_cleaned.rds"))
