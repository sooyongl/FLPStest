# rm(list = ls())
library(rstan)
library(foreach)
library(doParallel)
library(Rmpi)

for(i in list.files("R", full.names = T, pattern = "r$")) source(i);rm(i)
source_funs <- ls()
rstan_options(auto_write = TRUE)
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

# condition ---------------------------------------------------------------
lvmodel <- c("rasch","2pl","gpcm","grm")
nsample <- c(500, 1000, 2000)
nitem   <- c(10)
fnitem  <- expand.grid(nsample=nsample, nitem=nitem, lvmodel=lvmodel)

nsample <- c(1000)
nitem   <- c(50, 200)
fnsize  <- expand.grid(nsample=nsample, nitem=nitem, lvmodel=lvmodel)
cond_table <- rbind(fnitem,fnsize)

cond_table <- expand.grid.df(cond_table,data.frame(linearity = c(T,F)))
cond_table <- expand.grid.df(cond_table,data.frame(ydist = c("n","t","t3")))
cond_table <- expand.grid.df(cond_table,data.frame(rep = 1:100))
set.seed(1);cond_table$seedn <- sample(1:99999, nrow(cond_table), replace = T)
# core setting ------------------------------------------------------------
cl <- getMPIcluster()
doParallel::registerDoParallel(cl)

# n_cores <- detectCores() - 2; print(n_cores)
# cl <- parallel::makeCluster(n_cores)
# doSNOW::registerDoSNOW(cl)

# condition filter --------------------------------------------------------
cond_table <- cond_table[cond_table$linearity == T & cond_table$ydist == "n", ]

cond_table <- cond_table[cond_table$lvmodel == "2pl",  ]

cond_table <- cond_table[cond_table$nitem %in% c(50) &
                             cond_table$nsample %in% c(500) &
                             cond_table$rep %in% c(1:30)
                           ,  ]

prior_choice <- "normal"

# cond_table_2 <- cond_table[cond_table$nitem %in% c(100) &
#                              cond_table$nsample %in% c(500)
#                            ,  ]
#
# cond_table <- rbind(cond_table_1, cond_table_2)

# nrow(cond_table)
# loop --------------------------------------------------------------------
oo <- foreach(
  i = 1:nrow(cond_table),
  .packages = c("rstan"),
  .errorhandling = 'pass',
  .export = source_funs
) %dopar% {

  #set.seed(cond_table$seedn[i])
  nsample     <- cond_table$nsample[i]
  nitem       <- cond_table$nitem[i]
  linearity   <- cond_table$linearity[i]
  ydist       <- cond_table$ydist[i]
  lvmodel     <- as.character(cond_table$lvmodel[i])
  rep         <- cond_table$rep[i]

  r2y = 0.2
  r2eta = 0.5

  # generate data ------------------------------------------
  sim_condition <- list(
    N       = nsample, # sample size
    R2Y     = 0.2, # 0.1 0.2 0.5
    R2eta   = 0.5, # 0.2 0.5 0.75
    omega   = round(runif(1, 0.1, 0.3),3),
    tau0    = round(runif(1, 0.2, 0.4),3),
    tau1    = round(runif(1, -0.2, -0.1),3),
    linear  = linearity,
    ydist   = ydist,
    lambda  = 0.6,
    nsec    = nitem,
    nfac    = 1,
    lvmodel = lvmodel
  )

  # sdat <- do.call("makeDat", sim_condition)
  sdat <- do.call("makeDat", sim_condition)

  # run stan ----------------------------------------------------------------
  stanfilename <- paste0("inst/stan/ps", toupper(ifelse(lvmodel=="2pl", "irt",
                                                        lvmodel)),"_univ.stan")

  
  stanfilename <- stanfilename[grep(prior_choice,stanfilename)]
  
  if(tolower(lvmodel) != "rasch") {
    parselect <- c("eta","lambda","tau", "a1","b00","b0","b1","betaU","betaY")
  } else {
    parselect <- c("eta","tau", "a1","b00","b0","b1","betaU","betaY")
  }

  startt <- Sys.time()
  fit <- rstan::stan(
    file   = stanfilename,
    data   = sdat$stan_dt,
    chain  = 2,
    cores  = 1,
    iter   = 5000,
    warmup = 2000,
    include = T,
    pars = parselect
  )

  # Rhat(fit)
  # ess_bulk(fit)
  # ess_tail(fit)

  rstanend <- Sys.time()

  o <- list(fit=fit, sdat=sdat)

  filename <- paste0(paste(
    lvmodel, nsample, nitem, r2y, r2eta, linearity, ydist, rep,

    sep = "_"),".rds")

  saveRDS(o, file.path("results",filename))
  NA
  # res <- readRDS("results/2pl_500_100_0.2_0.5_TRUE_n_1.rds")
  # fit <- res$fit
  # sdat <- res$sdat

  # N <- sdat$N
  # stan_dt <- sdat$stan_dt
  # nb <- max(sdat$grad) - min(sdat$grad)
  # nfac <- sdat$nfac
  #
  # iter = fit@sim$iter - fit@sim$warmup
  # n.chain = fit@sim$chains
  #
  #
  # # df.fit <- as.data.frame(fit) %>% select(-matches("free"))
  # df.fit <- as.data.frame(fit)
  # if(sum(grep("free",names(df.fit))) == 0) {
  #   selected <- names(df.fit)
  # } else {
  #   selected <- names(df.fit)[!grepl("free",names(df.fit))]
  # }
  #
  # df.fit <- df.fit[selected]
  #
  # df.fit$chain <- rep(c(1:n.chain), each = iter)
  # # df.fit <- df.fit %>% group_by(chain)
  #
  # rm(o, fit)
  #
  # # population --------------------------------------------------------------
  # a11 <- sdat$omega
  # b0  <- sdat$tau0
  # b11 <- sdat$tau1
  # true_param <- c(-1, 0.5, 1.0, 0.5, 0, a11, b0, b11)
  # names(true_param) <- c("bu11","bu12","by1","by2","b00","a11", "b0", "b11")
  #
  # true_ipar <- sdat$lv.par
  # true_lam <- true_ipar[,(1:nfac)]
  #
  # true_tau <- true_ipar[,(nfac+1):ncol(true_ipar)]
  # true_tau <- true_tau[!grepl("g", names(true_tau))]
  # if(sdat$lvmodel != "2pl") {
  #   true_tau <- -true_tau
  # }
  # true_tau <- unlist(true_tau)
  #
  # true_eta  <- sdat$theta
  #
  # # estimates ---------------------------------------------------------------
  # # eta_raw    <- select(df.fit, chain, matches("^eta"))
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
  #   condition = filename,
  #   times = c(startt, rstanend),
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
  # saveRDS(o, paste0("results/cleaned/extracted_",filename ,".rds"))
  #
  #
  # names(o$eta_df) <- c("par_name","true_param","est_chain1","est_chain2","est_mean")
  # names(o$lambda_df) <- c("par_name","true_param","est_chain1","est_chain2","est_mean")
  # names(o$tau_df) <- c("par_name","true_param","est_chain1","est_chain2","est_mean")
  # names(o$struct_df) <- c("par_name","true_param","est_chain1","est_chain2","est_mean")
  #
  #
  # eta_df <- do.call("rbind", list(o$eta_df,o$lambda_df,o$tau_df,o$struct_df))
  # eta_df$err <- eta_df$est_mean - eta_df$true_param
  # eta_df$cond <- o$condition
  #
  # saveRDS(eta_df, paste0("results/cleaned/cleaned_",filename ,".rds"))
  #
  # cleanend<- Sys.time()
}
# stopCluster(cl)
