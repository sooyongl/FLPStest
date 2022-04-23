# rm(list = ls())
library(rstan)
# library(foreach)
# library(doParallel)
library(MplusAutomation);
library(tidyverse); library(ggforce)


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
cond_table <- expand.grid.df(cond_table,data.frame(rep = 1:50))

# core setting ------------------------------------------------------------
# cl <- getMPIcluster()
# doParallel::registerDoParallel(cl)

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

res <- vector("list", nrow(cond_table))
for(ct in 1:nrow(cond_table)) {
  print(ct)
  # ct = 1
  # set.seed(ct)
  nsample     <- cond_table$nsample[ct]
  nitem       <- cond_table$nitem[ct]
  linearity   <- cond_table$linearity[ct]
  ydist       <- cond_table$ydist[ct]
  lvmodel     <- as.character(cond_table$lvmodel[ct])
  rep         <- cond_table$rep[ct]

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

  N      <- sdat$N
  lambda <- sdat$lambda
  nsec   <- sdat$nsec

  Y <- sdat$stan_dt$Y
  Z <- sdat$stan_dt$Z
  X <- sdat$stan_dt$X

  ncov <- ncol(X)
  xname <- paste(paste0("X",1:ncov), collapse = " ")

  resp <- matrix(-99, N, ncol = nsec)
  for(i in 1:length(sdat$stan_dt$grad)) {
    # i <- 1
    w.item <- sdat$stan_dt$section[i]
    w.peop <- sdat$stan_dt$studentM[i]

    resp[w.peop, w.item]<- sdat$stan_dt$grad[i]

  }
  # tail(resp)
  dat <- cbind(Y, Z, X, resp)

  write.table(dat, "test/test_mplus/data_results/test_data.csv",
              row.names=FALSE, col.names=FALSE, sep=",")

  inpfile <- glue::glue("
data: file is test_data.csv;
variable:
  names are Y Z {xname} item1-item30;
  usevariable are {xname} Z Y item1-item30;
  categorical = item1-item30;

  missing = all (-99);

analysis:
   type = random;
   !estimator = ML;

model:

   F1 by item1-item30;

   [item1$1-item30$1];

   F1 on {xname};
   Y on Z {xname} F1;
   F1Z | F1 xwith Z;
   Y on F1Z;

")

  writeLines(inpfile, "test/test_mplus/data_results/test.inp")
  runModels("test/test_mplus/data_results/test.inp")


  filename <- paste0(paste(
    "Mplus",
    nsample, nitem, lvmodel, linearity, ydist, rep,

    sep = "_"))

  file.rename("test/test_mplus/data_results/test.out",
              paste0("test/test_mplus/data_results/",
                     paste0(filename, ".out")))


  res[[ct]] <- cbind(filename, sdat$lv.par)
}
res <- do.call("rbind", res)
saveRDS(res, "test/test_mplus/0416_population_value.rds")


####################################################################
outfiles <- fs::dir_ls("test/test_mplus/data_results", regexp = "out$")

popvalue <- readRDS("test/test_mplus/0416_population_value.rds")
res <- vector("list", length(outfiles))
for(x in 1:length(outfiles)) {
  print(x)

  # x = 33

  aaa <- str_split(outfiles[x], "/", simplify = T)[,4]
  aaa <- str_split(aaa, ".out", simplify = T)[,1]

  p.ipar <- popvalue %>% filter(filename == aaa)

  res_mplus<- readModels(outfiles[x])

  unstandardized <- res_mplus$parameters$unstandardized

  if(is.null(unstandardized))
    next

  lambda <- unstandardized %>%
    filter(str_detect(paramHeader, "BY")) %>%
    select(paramHeader, param, est) %>%
    mutate(param = str_replace(param, "ITEM", "lambda"))

  tau <- unstandardized %>%
    filter(str_detect(paramHeader, "Thre")) %>%
    select(paramHeader, param, est) %>%
    mutate(
      param = str_remove(param, "\\$1"),
      param = str_replace(param, "ITEM", "tau")
    )

  coeffs <- unstandardized %>%
    filter(str_detect(paramHeader, "ON")) %>%
    select(paramHeader, param, est) %>%
    mutate(
      param = c("bu11","bu12", "a11", "b11", "b0", "by1" ,"by2")
    )

  b0  = 0.3
  b11 = -0.15
  a11 = 0.2

  true_param <- c(-1, 0.5, a11, b11, b0, 1, 0.5)
  names(true_param) <- c("bu11","bu12","a11", "b11", "b0", "by1", "by2")

  coeffs <- coeffs %>% mutate(true_param=true_param)

  true_ipar <- p.ipar[, -1]
  true_lam <- true_ipar[,(1:1)]

  lambda <- lambda %>% mutate(true_param=true_lam)

  true_tau <- true_ipar[,(1+1):ncol(true_ipar)]
  true_tau <- true_tau[!grepl("g", names(true_tau))]
  true_tau <- -true_tau

  true_tau <- unlist(true_tau)
  tau <- tau %>% mutate(true_param=true_tau)

  res[[x]] <- bind_rows(coeffs, lambda, tau) %>%
    mutate(condition = aaa)
}

res <- res[!sapply(res, is.null)]
res <- do.call("rbind",res)
saveRDS(res, "test/test_mplus/0416_mplus_res.rds")

