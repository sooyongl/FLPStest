rm(list = ls())
library(rstan); library(MplusAutomation); library(tidyverse); library(ggforce)

for(i in list.files("R", full.names = T, pattern = "r$")) source(i)
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

lvmodel <- c("2pl")
nsample <- c(100, 500, 1000)
nitem   <- c(10, 50, 100)
fnitem  <- expand.grid(nsample=nsample, nitem=nitem, lvmodel=lvmodel)

cond_table <- expand.grid.df(fnitem,data.frame(rep = 1:20))


for(x in 1:nrow(cond_table)) {
  print(x)

  nsample     <- cond_table$nsample[x]
  nitem       <- cond_table$nitem[x]
  lvmodel     <- as.character(cond_table$lvmodel[x])
  rep         <- cond_table$rep[x]

  aaa <- paste(lvmodel, nsample, nitem, rep, sep = "_")

  # generate data ------------------------------------------
  sim_condition <- list(
    N       = nsample, # sample size
    R2Y     = 0.2, # 0.1 0.2 0.5
    R2eta   = 0.5, # 0.2 0.5 0.75
    omega   = 0.2,
    tau0    = 0.3,
    tau1    = -0.15,
    linear  = T,
    ydist   = 'n',
    lambda  = 0.6,
    nsec    = nitem,
    nfac    = 1,
    lvmodel = lvmodel
  )

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

  dat <- cbind(Y, Z, X, resp)

  write.table(dat, "test/test_mplus/data_results/test_data.csv",
              row.names=FALSE, col.names=FALSE, sep=",")

  inpfile <- glue::glue("
data: file is test_data.csv;
variable:
  names are Y Z {xname} item1-item{nsec};
  usevariable are {xname} Z Y item1-item{nsec};
  categorical = item1-item{nsec};

  missing = all (-99);

analysis:
   type = random;
   !estimator = ML;

model:

   F1 by item1-item{nsec};

   [item1$1-item{nsec}$1];

   F1 on {xname};
   Y on Z {xname} F1;
   F1Z | F1 xwith Z;
   Y on F1Z;

")

  writeLines(inpfile, "test/test_mplus/data_results/test.inp")
  runModels("test/test_mplus/data_results/test.inp")

  saveRDS(sdat, paste0("test/test_mplus/data_results/",
                       paste0(aaa, ".rds")))

  file.rename("test/test_mplus/data_results/test.out",
              paste0("test/test_mplus/data_results/",
                     paste0(aaa, ".out")))

}

###################
###################
###################
###################
###################
###################
outfiles <- fs::dir_ls("test/test_mplus/data_results", regexp = "out$")

library(foreach)

# res <- vector("list", length(outfiles))
res <- foreach(x = 1:length(outfiles),
               .errorhandling = "remove") %do% {
  print(x)

  # x = 1

  aaa <- str_split(outfiles[x], "/", simplify = T)[,4]
  aaa <- str_split(aaa, ".out", simplify = T)[,1]

  res_mplus <- readModels(outfiles[x])

  unstandardized <- res_mplus$parameters$unstandardized

  lambda <- unstandardized %>%
    filter(str_detect(paramHeader, "BY")) %>%
    select(paramHeader, param, est)

  tau <- unstandardized %>%
    filter(str_detect(paramHeader, "Thre")) %>%
    select(paramHeader, param, est)

  coeffs <- unstandardized %>%
    filter(str_detect(paramHeader, "ON")) %>%
    select(paramHeader, param, est)


  sdat <- readRDS(str_replace(outfiles[x], "\\.out", "\\.rds"))

  b0  = sdat$tau0
  b11 = sdat$tau1
  a11 = sdat$omega

  true_param <- c(-1, 0.5, a11, b11, b0, 1, 0.5)
  names(true_param) <- c("bu11","bu12","a11", "b11", "b0", "by1", "by2")


  coeffs <- coeffs %>% mutate(true_param=true_param)

  true_ipar <- sdat$lv.par
  true_lam <- true_ipar[,(1:1)]

  lambda <- lambda %>% mutate(true_param=true_lam)

  true_tau <- true_ipar[,(1+1):ncol(true_ipar)]
  true_tau <- true_tau[!grepl("g", names(true_tau))]
  true_tau <- -true_tau

  true_tau <- unlist(true_tau)
  tau <- tau %>% mutate(true_param=true_tau)

  #res[[x]] <-
  bind_rows(coeffs, lambda, tau) %>%
    mutate(
      est = case_when(str_detect(est, "\\*") ~ -9999,
                      TRUE ~ est),
      est = as.numeric(est),

      condition = aaa)
}

# res0000 <- lapply(res[51:100], function(x) {
#   x %>%
#     mutate(est = case_when(str_detect(est, "\\*") ~ -9999,
#                            TRUE ~ est),
#            est = as.numeric(est)
#     )
# })


# do.call("rbind", res)
res00 <- bind_rows(res)
saveRDS(res00, "test/test_mplus/0621_mplus_res.rds")



####################################################################
res00 <- readRDS("test/test_mplus/0621_mplus_res.rds")

res01 <- res00 %>%
  mutate(
    paramHeader = case_when(
      str_detect(paramHeader, "F1.BY") ~ "lambda",
      str_detect(paramHeader, "Thresholds") ~ "tau",
      TRUE ~ paramHeader
    ),
    param =
      case_when(
        str_detect(param, "ITEM[0-9]+$") ~ paramHeader,
        str_detect(param, "ITEM[0-9]+\\$1") ~ paramHeader,

        TRUE ~ paste0(paramHeader,param)
      )


  ) %>%

  separate(condition, c("lvmodel", "samplesize","nitem","rep"),
           sep = "_") %>%

  # filter(!str_detect(param, "Y\\.ONX")) %>%

  rename("par_name" = "param") %>%

  mutate(err = est - true_param) %>%
  mutate(
    par_name =
      case_when(
        par_name == "F1.ONX1" ~ "bu11",
        par_name == "F1.ONX2" ~ "bu12",
        par_name == "Y.ONF1" ~ "a11",
        par_name == "Y.ONF1Z" ~ "b11",
        par_name == "Y.ONZ" ~ "b0",
        par_name == "Y.ONX1" ~ "by1",
        par_name == "Y.ONX2" ~ "by2",
        TRUE ~ par_name
      ),
    par_name = factor(
      par_name,
      levels = c("bu11","bu12","a11", "b11", "b0", "by1", "by2", "lambda","tau")
    )
  )

res01 %>%
  # filter(est > 100) %>%
  group_by(par_name, samplesize, nitem, rep) %>%
  summarise(
    err = mean(err)
  ) %>%
  mutate(samplesize =
           factor(samplesize, levels = c(100, 500, 1000))) %>%
  # filter(samplesize == 1000) %>%

  ggplot(aes(x = par_name, y = err)) +
  geom_violin(
    trim=F,
    fill = "skyblue", alpha = 1, color = NA) +
  ggforce::geom_sina(size = 2,
                     alpha = 0.5
  ) +
  geom_hline(yintercept = 0) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    alpha = 0.8,
    fill = "red"
  ) +
  facet_grid(samplesize ~ nitem) +
  scale_y_continuous(n.breaks = 10,
                     limits = c(-1, 1))


  scale_alpha_manual(values = c(.1, .1, .1, .1, .01, .01,
                                .1, .1, .1, .1, .1, .1, .1))


