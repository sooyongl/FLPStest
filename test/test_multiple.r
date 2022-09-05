# rm(list = ls())
library(rstan)
library(foreach)
library(doParallel)
# library(Rmpi)

for(i in list.files("R", full.names = T, pattern = "r$")) source(i);rm(i)
source_funs <- ls()
rstan_options(auto_write = TRUE)
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

# condition ---------------------------------------------------------------
lvmodel <- c("rasch","2pl","gpcm","grm")
nsample <- c(1000)
nitem   <- c(20)
fnitem  <- expand.grid(nsample=nsample, nitem=nitem, lvmodel=lvmodel)

nsample <- c(1000)
nitem   <- c(100)
fnsize  <- expand.grid(nsample=nsample, nitem=nitem, lvmodel=lvmodel)
cond_table <- rbind(fnitem,fnsize)

cond_table <- expand.grid.df(cond_table,data.frame(linearity = c(T,F)))
cond_table <- expand.grid.df(cond_table,data.frame(ydist = c("n","t","t3")))
cond_table <- expand.grid.df(cond_table,data.frame(rep = 220813001:220813100))
set.seed(1);cond_table$seedn <- sample(1:99999, nrow(cond_table), replace = T)
# core setting ------------------------------------------------------------
# cl <- getMPIcluster()
# doParallel::registerDoParallel(cl)

# n_cores <- detectCores() - 2; print(n_cores)
# cl <- parallel::makeCluster(n_cores)
# doSNOW::registerDoSNOW(cl)

# condition filter --------------------------------------------------------
cond_table <- cond_table[cond_table$linearity == T & cond_table$ydist == "n", ]

cond_table <- cond_table[cond_table$lvmodel == "gpcm", ]

cond_table <- cond_table[cond_table$nitem %in% c(20) &
                           cond_table$nsample %in% c(1000)
                         ,  ]

# cond_table_2 <- cond_table[cond_table$nitem %in% c(100) &
#                              cond_table$nsample %in% c(500)
#                            ,  ]
#
# cond_table <- rbind(cond_table_1, cond_table_2)

# nrow(cond_table)
# loop --------------------------------------------------------------------
# oo <- foreach(
#   i = 1:nrow(cond_table),
#   .packages = c("rstan"),
#   .errorhandling = 'pass',
#   .export = source_funs
# ) %dopar% {
  i = 1
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

  sdat$lv.par
  # temp1 <- simData.pcm(sdat$lv.par["a1"], sdat$lv.par[-1], theta = matrix(rnorm(5000)))
  # fitmirt<- mirt::mirt(dat = data.frame(temp1), model = 1, itemtype = "gpcm")
  # mirt::coef(fitmirt, IRTpars = T, simplify = T)$items


  fitmirt<- mirt::mirt(dat = sdat$lv.resp, model = 1, itemtype = "gpcm")
  mirt::coef(fitmirt, IRTpars = T, simplify = T)$items

  apply(sdat$lv.resp[,], 2, table)

  # run stan ----------------------------------------------------------------
  # stanfilename <- paste0("inst/stan/ps", toupper(ifelse(lvmodel=="2pl", "irt",
                                                        # lvmodel)),"_univ.stan")
  # stan_model <- paste(readLines(stanfilename), collapse = "\n")
  #
  # stanmodel_obj <- stan_model(model_code = stan_model,
  #                             model_name = gsub("inst/stan/", "", stanfilename))

  # saveRDS(stanmodel_obj, gsub("\\.stan", "\\.rds", stanfilename ))

  # stanmodel_obj <- readRDS(gsub("\\.stan", "\\.rds", stanfilename ))

  # stanmodel_obj <- readRDS("inst/stan/tacc_GRM_univ.rds")

  stanmodel_obj <- readRDS("inst/stan/tacc_GPCM_univ.rds")

  # stanmodel_obj <- readRDS("C:/Users/sooyong/Box/git/stancode_backup_4/psGPCM_univ.rds")

  # stanfilename <- stanfilename[grep("normal",stanfilename)]

  if(tolower(lvmodel) != "rasch") {
    parselect <- c("eta","lambda","tau", "a1","b00","b0","b1","betaU","betaY")
  } else {
    parselect <- c("eta","tau", "a1","b00","b0","b1","betaU","betaY")
  }

  startt <- Sys.time()
  # fit <- rstan::stan(
  fit <- rstan::sampling(
    # file = "inst/stan/tacc_GPCM_univ.stan",
    object   = stanmodel_obj,
    # model_name = "",
    # model_code = stan_model,
    data   = sdat$stan_dt,
    chain  = 1,
    cores  = 1,
    iter   = 3000,
    warmup = 1000,
    include = T,
    pars = parselect
  )

  # Rhat(fit)
  # ess_bulk(fit)
  # ess_tail(fit)
  # summary(fit)

  rstanend <- Sys.time()

  o <- list(fit=fit, sdat=sdat)

  filename <- paste0(paste(
    lvmodel, nsample, nitem, r2y, r2eta, linearity, ydist, rep,

    sep = "_"),".rds")

  saveRDS(o, file.path("results",filename))
  # saveRDS(o, file.path("results",filename))
  NA

  # NA
# }
# stopCluster(cl)
#
# rstanend - startt
# cleanend - rstanend
