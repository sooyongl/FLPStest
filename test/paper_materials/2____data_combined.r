# rm(list = ls())
library(tidyverse)
library(rstan)
library(ggpubr)
library(cowplot)
library(ggforce)
library(gt)

data_idx <- "0903"

# FULL replications ------------------------------------------
total_res <- readRDS(paste0("test/shinydata/", data_idx,"_data.rds"))
mpart <- readRDS(paste0("test/shinydata/", data_idx, "_mpart.rds")) %>%
  rename(est_mean = mean) %>%
  separate(
    cond, c("lvmodel","samplesize","nitem","z1","z2","z3","z4","rep"),
    sep = "_"
  ) %>% select(-starts_with("z"))

pick_100 <- total_res %>%
  ungroup() %>%
  filter(lvmodel %in% c("rasch","2pl","grm","gpcm")) %>%
  distinct(lvmodel, samplesize, nitem, rep) %>%
  group_split(lvmodel, samplesize, nitem) %>%
  map_df(., ~ .x[1:100, ])

pick_prior <- total_res %>%
  ungroup() %>%
  filter(!lvmodel %in% c("rasch","2pl","grm","gpcm")) %>%
  distinct(lvmodel, samplesize, nitem, rep)

pick_100 <- bind_rows(pick_100, pick_prior)

total_res <- pick_100 %>%
  left_join(total_res,
            by = c("lvmodel","samplesize","nitem","rep"))

mpart <- pick_100 %>%
  left_join(mpart,
            by = c("lvmodel","samplesize","nitem","rep"))

# -----------------------------------------------
mpart0 <- mpart %>%
  mutate(
    par_name0 = case_when(
      str_detect(par_name, "eta") ~ "eta",
      str_detect(par_name, "lambda") ~ "lambda",
      str_detect(par_name, "tau") ~ "tau"
    ),
    # err = est_mean - true_param,
    # rerr = (est_mean - true_param)/true_param,
    #
    coverage =
      case_when(
        true_param > X2.5. & true_param < X97.5. ~ 1,
        TRUE ~ 0
      ),
    converge_Rhat =
      case_when(
        Rhat > 1.1  ~ 0,
        TRUE ~ 1
      )
  ) %>%
  select(lvmodel, samplesize, nitem, rep, par_name, par_name0,
         true_param, est_mean,
         n_eff,
         Rhat,
         err,
         rerr,
         coverage,
         converge_Rhat)

xxxx <- mpart0 %>% distinct(lvmodel,samplesize, nitem, rep)
trt <- c()
for(i in 1:nrow(xxxx)) {
  print(i)
  ss <- xxxx[i,] %>% pull(samplesize) %>% as.numeric()
  ni <- xxxx[i,] %>% pull(nitem) %>% as.numeric()
  lv <- xxxx[i,] %>% pull(lvmodel)

  if(lv == "rasch") {
    trt <- c(trt, c(rep(c("1","0"), each = ss/2),
                    rep(c(""), each = ni)))
  } else if(lv %in% c("gpcm","grm")) {
    trt <- c(trt, c(rep(c("1","0"), each = ss/2),
                    rep(c("","","",""), each = ni)))

  } else {
    trt <- c(trt, c(rep(c("1","0"), each = ss/2),
                    rep(c("",""), each = ni)))
  }
}
# length(trt)
mpart0$trt <- trt

mpart0 <- mpart0 %>%
  filter(!is.nan(Rhat)) %>%
  mutate(par_name0 = paste0(par_name0, trt))

mpart_by_rep <- mpart0 %>%
  group_by(lvmodel, samplesize, nitem, rep, par_name0) %>%
  summarise(
    bias   = mean(err),
    rbias  = mean(rerr),

    absbias = mean(abs(err)),
    rmse = sqrt(mean(err^2)),
    coverage = mean(coverage),

    Rhatconv = mean(converge_Rhat),
    Rhatmean = mean(Rhat)
  )


mpart_by_cond <- mpart_by_rep %>%
  group_by(lvmodel, samplesize, nitem, par_name0) %>%
  summarise(
    bias   = mean(bias),
    rbias   = mean(rbias),
    absbias = mean(absbias),
    rmse = mean(rmse),
    coverage = mean(coverage),
    Rhatconv = mean(Rhatconv),
    Rhatmean = mean(Rhatmean)
  )

mpart_by_cond <- mpart_by_cond %>%
  mutate(
    lvmodel = case_when(
      lvmodel == "rasch" ~ "Rasch",
      lvmodel == "2pl" ~ "2PL",
      lvmodel == "gpcm" ~ "GPCM",
      lvmodel == "grm" ~ "GRM",
      lvmodel == "2pllogn" ~ "2PL.LogN",
      lvmodel == "2plnormal" ~ "2PL.Normal",
      lvmodel == "2plunif" ~ "2PL.Unif"
    ),
    lvmodel = factor(lvmodel,
                     c("Rasch","2PL","GRM","GPCM",
                       "2PL.LogN","2PL.Normal","2PL.Unif")),

    par_name0 = case_when(
      par_name0 == "eta1" ~ "eta1",
      par_name0 == "eta0" ~ "eta0",
      par_name0 == "lambda" ~ "a",
      par_name0 == "tau" ~ "b"
    ),
    par_name0 = factor(par_name0,
                       c("eta1","eta0","a","b")),

    samplesize = factor(samplesize, c("500","1000","2000")),
    nitem = factor(nitem, c("50","100","200"))
  ) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~ round(.x, 3))


saveRDS(mpart0, "test/paper_materials/mpart0.rds")
saveRDS(mpart_by_rep, "test/paper_materials/mpart_by_rep.rds")
saveRDS(mpart_by_cond, "test/paper_materials/mpart_by_cond.rds")


# Structure model -----------------------------------------
total_res <-   total_res %>%
  mutate(
    par_name = as.character(par_name),
    par_name = case_when(
      str_detect(par_name, "bu") ~ "bu",
      str_detect(par_name, "by") ~ "by",
      TRUE ~ par_name
    ),
    par_name = factor(par_name,
                      levels = c("b00","b0", "b11" ,"a11","bu","by",
                                 "lambda","tau","eta"))
  ) %>%
  filter(!par_name %in% c("lambda","tau","eta"))

total_res

saveRDS(., "test/paper_materials/total_res.rds")


total_res1 <-
  total_res %>%
  group_by(lvmodel, samplesize, nitem, par_name, rep) %>%
  summarise(
    err = mean(value),
    rerr = mean(rvalue),

    bias = mean(value),
    rbias = mean(rvalue),

    X2.5. = mean(X2.5.),
    X97.5. = mean(X97.5.),
    coverage = mean(coverage),
    Rhat = mean(Rhat)
  ) %>%
  mutate(
    abserr = abs(err),
    absrerr = abs(rerr),

    absbias = abs(err),
    absrbias = abs(rerr)
  ) %>%
  mutate_if(is.numeric, ~ round(.x, 3))


total_res2 <- total_res1 %>%
  group_by(lvmodel, samplesize, nitem, par_name) %>%
  summarise(
    N = n(),
    bias = mean(err),
    absbias = mean(abs(err)),

    rbias = mean(rerr),
    absrbias = mean(abs(rerr)),

    rmse = sqrt(mean(err^2)),
    absrmse = sqrt(mean(abs(err)^2)),

    absrrmse = sqrt(mean(rerr^2)),
    absrrmse = sqrt(mean(abs(rerr)^2)),

    VAR.bias = var(err),

    X2.5. = mean(X2.5.),
    X97.5. = mean(X97.5.),
    coverage = mean(coverage),
    Rhat = mean(Rhat)
  ) %>%
  ungroup() %>%
  mutate(
    MSE.bias = bias^2
  ) %>%
  arrange(lvmodel, nitem, par_name, samplesize)

total_res3 <- total_res2 %>%
  mutate(
    lvmodel = case_when(
      lvmodel == "rasch" ~ "Rasch",
      lvmodel == "2pl" ~ "2PL",
      lvmodel == "gpcm" ~ "GPCM",
      lvmodel == "grm" ~ "GRM",
      lvmodel == "2pllogn" ~ "2PL.LogN",
      lvmodel == "2plnormal" ~ "2PL.Normal",
      lvmodel == "2plunif" ~ "2PL.Unif"
    ),
    lvmodel = factor(lvmodel,
                     c("Rasch","2PL","GRM","GPCM",
                       "2PL.LogN","2PL.Normal","2PL.Unif")),

    samplesize = factor(samplesize, c("500","1000","2000")),
    nitem = factor(nitem, c("50","100","200"))
  ) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~ round(.x, 3))

saveRDS(total_res1, "test/paper_materials/total_res1.rds")
saveRDS(total_res2, "test/paper_materials/total_res2.rds")
saveRDS(total_res3, "test/paper_materials/total_res3.rds")


# Convergence tets -------------------------------------
mpart0 <- readRDS("test/paper_materials/mpart0.rds")
total_res1 <- readRDS("test/paper_materials/total_res1.rds")

rep_type_combined <- mpart0 %>%
  filter(!lvmodel %in% c("2pllogn","2plnormal","2plunif")) %>%
  select(lvmodel, samplesize, nitem, rep, par_name = par_name0, Rhat) %>%
  bind_rows(total_res1 %>%
              filter(!lvmodel %in% c("2pllogn","2plnormal","2plunif")) %>%
              select(lvmodel, samplesize, nitem, rep, par_name, Rhat)
            )

converged_results0 <- rep_type_combined %>%
  mutate(conv = ifelse(Rhat > 1.1, "nonc", "conv")) %>%
  mutate(
    lvmodel = case_when(
      lvmodel == "rasch" ~ "Rasch",
      lvmodel == "2pl" ~ "2PL",
      lvmodel == "gpcm" ~ "GPCM",
      lvmodel == "grm" ~ "GRM"
      # lvmodel == "2pllogn" ~ "2PL.LogN",
      # lvmodel == "2plnormal" ~ "2PL.Normal",
      # lvmodel == "2plunif" ~ "2PL.Unif"
    ),
    lvmodel = factor(lvmodel,
                     c("Rasch","2PL","GRM","GPCM"#,
                       #"2PL.LogN","2PL.Normal","2PL.Unif"
                     )),

    samplesize = factor(samplesize, c("500","1000","2000")),
    nitem = factor(nitem, c("50","100","200"))
  ) %>% #print(n = 10000)
  filter(!is.na(conv))

saveRDS(converged_results0, "test/paper_materials/converged_results0.rds")

converged_results1 <-
  converged_results0 %>%
  filter(conv == "nonc") %>%
  group_by(lvmodel, samplesize, nitem, rep) %>%
  count(lvmodel, samplesize, nitem)

nonc <- converged_results1 %>%
  mutate(
    condd = paste(lvmodel, samplesize, nitem, rep, sep = "_")
  ) %>% pull(condd)


converged_results2 <-
  converged_results0 %>%
  mutate(conved = ifelse(conv == "conv", 1, 0)) %>%
  group_by(lvmodel, samplesize, nitem, rep) %>%
  summarise(conv = mean(conved)) %>%
  mutate(conv = ifelse(conv == 1, "conv","nonc")) %>%

  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  mutate(lvmodel = case_when(
    lvmodel == "Rasch" ~ "rasch",
    lvmodel == "2PL" ~ "2pl",
    lvmodel == "GPCM" ~ "gpcm",
    lvmodel == "GRM" ~ "grm"))



# Combined ------------------------------------------

mpart_by_rep <- readRDS("test/paper_materials/mpart_by_rep.rds")
total_res1 <- readRDS("test/paper_materials/total_res1.rds")

temp1 <- mpart_by_rep %>%

  left_join(converged_results2,
            by = c("lvmodel","samplesize","nitem", "rep")
  ) %>%
  filter(conv == "conv") %>%
  group_by(lvmodel, samplesize, nitem, rep) %>%
  summarise(n = n()) %>%
  group_split() %>%
  map_df(., ~ .x[1:nrow(.x), ]) %>%
  mutate(keep100 = "y") %>% select(-n)

mpart_by_cond <- mpart_by_rep %>%
  left_join(temp1,
            by = c("lvmodel","samplesize","nitem", "rep")
  ) %>%
  filter(keep100 == "y") %>%
  group_by(lvmodel, samplesize, nitem, par_name0) %>%
  summarise(
    bias   = mean(bias),
    rbias   = mean(rbias),
    absbias = mean(absbias),
    rmse = mean(rmse),
    coverage = mean(coverage),
    Rhatconv = mean(Rhatconv),
    Rhatmean = mean(Rhatmean)
  )

mpart_by_cond <- mpart_by_cond %>%
  mutate(
    lvmodel = case_when(
      lvmodel == "rasch" ~ "Rasch",
      lvmodel == "2pl" ~ "2PL",
      lvmodel == "gpcm" ~ "GPCM",
      lvmodel == "grm" ~ "GRM",
      lvmodel == "2pllogn" ~ "2PL.LogN",
      lvmodel == "2plnormal" ~ "2PL.Normal",
      lvmodel == "2plunif" ~ "2PL.Unif"
    ),
    lvmodel = factor(lvmodel,
                     c("Rasch","2PL","GRM","GPCM",
                       "2PL.LogN","2PL.Normal","2PL.Unif")),

    par_name0 = case_when(
      par_name0 == "eta1" ~ "theta1",
      par_name0 == "eta0" ~ "theta0",
      par_name0 == "lambda" ~ "a",
      par_name0 == "tau" ~ "b"
    ),
    par_name0 = factor(par_name0,
                       c("theta1","theta0","a","b")),

    samplesize = factor(samplesize, c("500","1000","2000")),
    nitem = factor(nitem, c("50","100","200"))
  ) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~ round(.x, 3))

saveRDS(mpart_by_rep, "test/paper_materials/mpart_by_rep_convadd.rds")
saveRDS(mpart_by_cond, "test/paper_materials/mpart_by_cond_convadd.rds")

####
temp1 <- total_res1 %>%

  left_join(converged_results2,
            by = c("lvmodel","samplesize","nitem", "rep")
  ) %>%
  filter(conv == "conv") %>%
  group_by(lvmodel, samplesize, nitem, rep) %>%
  summarise(n = n()) %>%
  group_split() %>%
  map_df(., ~ .x[1:nrow(.x), ]) %>%
  mutate(keep100 = "y") %>% select(-n)

total_res2 <- total_res1 %>%
  left_join(temp1,
            by = c("lvmodel","samplesize","nitem", "rep")
  ) %>%
  filter(keep100 == "y") %>%
  group_by(lvmodel, samplesize, nitem, par_name) %>%
  summarise(
    N = n(),
    bias = mean(err),
    absbias = mean(abs(err)),

    rbias = mean(rerr),
    absrbias = mean(abs(rerr)),

    rmse = sqrt(mean(err^2)),
    absrmse = sqrt(mean(abs(err)^2)),

    absrrmse = sqrt(mean(rerr^2)),
    absrrmse = sqrt(mean(abs(rerr)^2)),

    VAR.bias = var(err),

    X2.5. = mean(X2.5.),
    X97.5. = mean(X97.5.),
    coverage = mean(coverage),
    Rhat = mean(Rhat)
  ) %>%
  ungroup() %>%
  mutate(
    MSE.bias = bias^2
  ) %>%
  arrange(lvmodel, nitem, par_name, samplesize)

total_res3 <- total_res2 %>%
  mutate(
    lvmodel = case_when(
      lvmodel == "rasch" ~ "Rasch",
      lvmodel == "2pl" ~ "2PL",
      lvmodel == "gpcm" ~ "GPCM",
      lvmodel == "grm" ~ "GRM",
      lvmodel == "2pllogn" ~ "2PL.LogN",
      lvmodel == "2plnormal" ~ "2PL.Normal",
      lvmodel == "2plunif" ~ "2PL.Unif"
    ),
    lvmodel = factor(lvmodel,
                     c("Rasch","2PL","GRM","GPCM",
                       "2PL.LogN","2PL.Normal","2PL.Unif")),

    samplesize = factor(samplesize, c("500","1000","2000")),
    nitem = factor(nitem, c("50","100","200"))
  ) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~ round(.x, 3))

saveRDS(total_res1, "test/paper_materials/total_res1_convadd.rds")
saveRDS(total_res2, "test/paper_materials/total_res2_convadd.rds")
saveRDS(total_res3, "test/paper_materials/total_res3_convadd.rds")





