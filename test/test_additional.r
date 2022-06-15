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
# lvmodel <- c("rasch","2pl","gpcm","grm")
# nsample <- c(500, 1000, 2000)
# nitem   <- c(10)
# fnitem  <- expand.grid(nsample=nsample, nitem=nitem, lvmodel=lvmodel)
#
# nsample <- c(1000)
# nitem   <- c(50, 200)
# fnsize  <- expand.grid(nsample=nsample, nitem=nitem, lvmodel=lvmodel)
# cond_table <- rbind(fnitem,fnsize)
#
# cond_table <- expand.grid.df(cond_table,data.frame(linearity = c(T,F)))
# cond_table <- expand.grid.df(cond_table,data.frame(ydist = c("n","t","t3")))
# cond_table <- expand.grid.df(cond_table,data.frame(rep = 1:100))
# set.seed(1);cond_table$seedn <- sample(1:99999, nrow(cond_table), replace = T)
# core setting ------------------------------------------------------------
# cl <- getMPIcluster()
# doParallel::registerDoParallel(cl)

# n_cores <- detectCores() - 2; print(n_cores)
# cl <- parallel::makeCluster(n_cores)
# doSNOW::registerDoSNOW(cl)

# condition filter --------------------------------------------------------
a1 <- list.files("results", pattern = "rds")
a1 <- a1[!grepl("normal|unif", a1)]
a1 <- table(unlist(strsplit(a1,"\\d+.rds", perl = T)))
a1 <- 100 - a1
a1 <- a1[a1 > 0]

filename0 <- names(a1)
filename0 <- do.call("rbind", strsplit(filename0, "_"))
filename0 <- filename0[, c(2, 3, 1, 6, 7)]
filename0 <- apply(filename0, 1, function(x) paste(x[1],x[2],x[3],x[4],x[5], sep = "_"))

additional_rep <- c()
for(i in 1:length(a1)) {
  additional_rep <- c(additional_rep,
                      paste0(filename0[i], "_",
                             1:a1[i]))
}

additional_rep <- data.frame(additional_rep)
additional_rep$additional_rep <- as.character(additional_rep$additional_rep)
conditions <- do.call("rbind", strsplit(additional_rep$additional_rep, "_"))
conditions <- data.frame(conditions)
conditions$X1 <- as.character(conditions$X1)
conditions$X1 <- as.numeric(conditions$X1)

conditions$X2 <- as.character(conditions$X2)
conditions$X2 <- as.numeric(conditions$X2)

conditions$X3 <- as.character(conditions$X3)

conditions$X4 <- as.character(conditions$X4)
conditions$X4 <- as.logical(conditions$X4)

conditions$X6 <- as.character(conditions$X6)
conditions$X6 <- as.numeric(conditions$X6)
conditions$X6 <- conditions$X6 + 66600
#
names(conditions) <- c("nsample","nitem","lvmodel","linearity","ydist", "rep")

cond_table <- conditions
cond_table

cond_table <- cond_table[cond_table$lvmodel == "rasch", ]

# cond_table <- readRDS("cond_table_add.r")

# nrow(cond_table)
# loop -----------------------------------------------------------
oo <- foreach(
  i = 1:nrow(cond_table),
  .packages = c("rstan"),
  .errorhandling = 'pass',
  .export = source_funs
) %do% {

  #set.seed(cond_table$seedn[i])
  nsample     <- cond_table$nsample[i]
  nitem       <- cond_table$nitem[i]
  linearity   <- as.logical(cond_table$linearity[i])
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

}
# stopCluster(cl)
