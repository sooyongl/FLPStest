rm(list = ls())
library(rstan); library(MplusAutomation); library(tidyverse); library(ggforce)

for(i in list.files("R", full.names = T, pattern = "r$")) source(i)

files <- fs::dir_ls("test/check_data_generation/results")

for(x in 1:length(files)) {
  print(x)
  # x = 1
  aaa <- str_split(files[x], "/", simplify = T)[,4]
  aaa <- str_split(aaa, ".rds", simplify = T)[,1]

  a1 <- readRDS(files[x])
  sdat <-a1$sdat

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
  names are Y Z {xname} item1-item20;
  usevariable are {xname} Z Y item1-item20;
  categorical = item1-item20;

  missing = all (-99);

analysis:
   type = random;
   !estimator = ML;

model:

   F1 by item1-item20;

   [item1$1-item20$1];

   F1 on {xname};
   Y on Z {xname} F1;
   F1Z | F1 xwith Z;
   Y on F1Z;

")

  writeLines(inpfile, "test/test_mplus/data_results/test.inp")
  runModels("test/test_mplus/data_results/test.inp")

  file.rename("test/test_mplus/data_results/test.out",
              paste0("test/test_mplus/data_results/",paste0(aaa, ".out")))

}

###################
###################
###################
###################
###################
###################
outfiles <- fs::dir_ls("test/test_mplus/data_results", regexp = "out$")

res <- vector("list", length(outfiles))
for(x in 1:length(outfiles)) {
  print(x)

  # x = 1

  aaa <- str_split(outfiles[x], "/", simplify = T)[,4]
  aaa <- str_split(aaa, ".out", simplify = T)[,1]

  res_mplus<- readModels(outfiles[x])

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


  rds <- paste0("test/check_data_generation/results/",paste0(aaa, ".rds"))
  a1 <- readRDS(rds)
  sdat <-a1$sdat

  X <- sdat$stan_dt$X
  ncov <- ncol(X)

  b0  = sdat$tau0
  b11 = sdat$tau1
  a11 = sdat$omega

  if(ncov==2) {

    true_param <- c(-1, 0.5, a11, b11, b0, 1, 0.5)
    names(true_param) <- c("bu11","bu12","a11", "b11", "b0", "by1", "by2")
  } else {
    true_param <- c(-1, 0.5, 1.0, -0.5, a11, b11, b0, 1, 0.5, -1,-0.5)
    names(true_param) <- c("bu11","bu12","bu13","bu14",
                           'a11', "b11", "b0", "by1", "by2", "by3", "by4")
  }

  coeffs <- coeffs %>% mutate(true_param=true_param)

  true_ipar <- sdat$lv.par
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

res <- bind_rows(res)
saveRDS(res, "test/test_mplus/0413_mplus_res.rds")



####################################################################
res <- readRDS("test/test_mplus/0413_mplus_res.rds")

res %>%
  # filter(str_detect(condition, "logn")) %>%
  mutate(
    condition_1 = case_when(
      str_detect(condition, "compl") ~ "complete",
      TRUE ~ "missing"
    ),
    paramHeader = case_when(
      str_detect(paramHeader, "F1.BY") ~ "lambda",
      str_detect(paramHeader, "Thresholds") ~ "tau",
      TRUE ~ paramHeader
    ),
    param =
      case_when(
        str_detect(param, "ITEM[1-9]$|ITEM[10-20]") ~ paramHeader,
        str_detect(param, "ITEM[1-9]\\$1|ITEM[10-20]\\$1") ~ paramHeader,

        TRUE ~ paste0(paramHeader,param)
      )


  ) %>%

  separate(condition, c("condition", "a"), sep = "_2pl_") %>%
  select(-a) %>%

  filter(!str_detect(param, "Y\\.ONX")) %>%

  rename("par_name" = "param") %>%

  mutate(err = est - true_param) %>%

  ggplot(aes(x = par_name, y = err)) +
  geom_violin(
    trim=F,
    fill = "skyblue", alpha = 0.5, color = NA) +
  ggforce::geom_sina(size = 2,
                     # alpha = 0.5,
                     aes(alpha = par_name)
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
  facet_wrap(. ~ condition_1) +
  scale_y_continuous(n.breaks = 10) +
  scale_alpha_manual(values = c(.1, .1, .1, .1, .01, .01,
                                .1, .1, .1, .1, .1, .1, .1))


