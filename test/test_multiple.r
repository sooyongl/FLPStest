# rm(list = ls())
library(rstan)
library(foreach)
library(doParallel)
library(Rmpi)

for(i in list.files("R", full.names = T, pattern = "r$")) source(i);rm(i)
source_funs <- ls()

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

# condition ---------------------------------------------------------------
lvmodel <- c("rasch","2pl","gpcm","grm")
nsample <- c(100, 2000)
nitem   <- c(30)
fnitem  <- expand.grid(nsample=nsample, nitem=nitem, lvmodel=lvmodel)

nsample <- c(1000)
nitem   <- c(50, 200)
fnsize  <- expand.grid(nsample=nsample, nitem=nitem, lvmodel=lvmodel)
cond_table <- rbind(fnitem,fnsize)

cond_table <- expand.grid.df(cond_table,data.frame(linearity = c(T,F)))
cond_table <- expand.grid.df(cond_table,data.frame(ydist = c("n","t","t3")))
cond_table <- expand.grid.df(cond_table,data.frame(rep = 1:10))

# core setting ------------------------------------------------------------
cl <- getMPIcluster()
doParallel::registerDoParallel(cl)

# n_cores <- detectCores() - 2; print(n_cores)
# cl <- parallel::makeCluster(n_cores)
# doSNOW::registerDoSNOW(cl)

# condition filter --------------------------------------------------------
cond_table <-
  cond_table[
    cond_table$lvmodel == "2pl" & 
      cond_table$linearity == T & 
      cond_table$ydist == "n" & 
      cond_table$nitem %in% c(30),  ]
# nrow(cond_table)
# loop --------------------------------------------------------------------
oo <- foreach(
  i = 1:nrow(cond_table),
  .packages = c("rstan"),
  .errorhandling = 'pass',
  .export = source_funs
) %dopar% {

  # set.seed(i)
  nsample     <- cond_table$nsample[i]
  nitem       <- cond_table$nitem[i]
  linearity   <- cond_table$linearity[i]
  ydist       <- cond_table$ydist[i]
  lvmodel     <- as.character(cond_table$lvmodel[i])
  rep         <- cond_table$rep[i]

  # generate data ------------------------------------------
  sim_condition <- list(
    N       = nsample, # sample size
    R2Y     = 0.3, # 0.1 0.2 0.5
    R2eta   = 0.5, # 0.2 0.5 0.75
    omega   = 0.2, # round(runif(1, 0.1, 0.3),3),
    tau0    = 0.3, # round(runif(1, 0.2, 0.4),3),
    tau1    = -0.15, # round(runif(1, -0.2, -0.1),3),
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
  fit <- rstan::stan(
    file   = stanfilename,
    data   = sdat$stan_dt,
    chain  = 2,
    cores  = 1,
    iter   = 6000,
    warmup = 2000
  )

  o <- list(fit=fit, sdat=sdat)

  filename <- paste0(paste(
    "matched",
    nsample, nitem, lvmodel, linearity, ydist, rep,
    
    sep = "_"),".rds")

  saveRDS(o, file.path("results",filename))
  #sdat

  NA
}
# stopCluster(cl)
