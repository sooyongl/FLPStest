library(tidyverse)
data_idx <- "0903"

# -------------------------------------------------------------------------
mpart0 <- readRDS("test/paper_materials/mpart0.rds")
mpart_by_rep <- readRDS("test/paper_materials/mpart_by_rep.rds")
mpart_by_cond <- readRDS("test/paper_materials/mpart_by_cond.rds")

mpart0 %>%
  filter(lvmodel == "gpcm", samplesize == "2000",
         str_detect(par_name0, "tau"))

mpart0 %>%
  filter(lvmodel == "grm", samplesize == "500",
         str_detect(par_name0, "tau"))

mpart0 %>%
  filter(lvmodel == "gpcm", samplesize == "500",
         str_detect(par_name0, "tau")) %>%
  group_by(lvmodel, samplesize, par_name, rep) %>%
  summarise(
    bias = mean(err),
    RMSE = sqrt(mean(err^2)),
    coverage = mean(coverage)
  ) %>%
  arrange(rep, par_name)

mpart0 %>%
  filter(lvmodel == "gpcm", samplesize == "500",
         str_detect(par_name0, "tau")) %>%
  group_by(lvmodel, samplesize) %>%
  summarise(
    bias = mean(err),
    RMSE = sqrt(mean(err^2)),
    coverage = mean(coverage)
  )

mpart_by_cond %>%
  filter(lvmodel == "GPCM", samplesize == "500",
         str_detect(par_name0, "b"))

# -------------------------------------------------------------------------
# bayes_res0 <- readRDS(paste0("results/cleaned/",data_idx,"_extracted_cleaned.rds"))

bayes_res <- readRDS(paste0("results/cleaned/",data_idx,"_extracted_cleaned.rds"))
# bayes_res <- bayes_res %>% rename(true_param = true_parm)
# nrow(bayes_res)

bayes_res <- bayes_res %>%
  mutate(Rhat = case_when(is.nan(Rhat) ~ 1, TRUE ~ Rhat)) %>%
  # filter(is.na(Rhat))
  mutate(rerr = (err)/true_param, .after = err) %>%
  tibble()

bayes_res <- bayes_res %>% mutate_if(is.factor, as.character)

bayes_res <- bayes_res %>%
  mutate(par_name = case_when(
    par_name == "betaU[1]" ~ "bu11",
    par_name == "betaU[2]" ~ "bu12",
    par_name == "betaY[1]" ~ "by1",
    par_name == "betaY[2]" ~ "by2",
    par_name == "a1" ~ "a11",
    par_name == "b1" ~ "b11",
    TRUE ~ par_name
  ),
  CIin = case_when(
    true_param > X2.5. & true_param < X97.5. ~ 1,
    TRUE ~ 0
  )
  )


# 220815
# newdat <- bayes_res

grm <- bayes_res %>%
  filter(str_detect(cond, "grm"))

gpcm <- bayes_res %>%
  filter(str_detect(cond, "gpcm"))

gpcm_1 <- gpcm %>%
  filter(str_detect(par_name, "tau")) %>%
  separate(
    par_name,
    c("par_name", "twhich"),
    sep = ","
  )

gpcm_1 %>%
  filter(str_detect(cond, "1000_100")) %>%
  filter(CIin == 0)

gpcm_1 %>%
  # filter(prior == "15") %>%
  filter(str_detect(cond, "2000_100")) %>%
  filter(CIin == 0)


gpcm_1 %>%
  filter(str_detect(cond, "2000_100")) %>%

  # filter(CIin == 0)


  group_by(cond) %>%
  # group_by(twhich) %>%
  # group_by(par_name) %>%
  summarise(mean(CIin)) %>%
  print(n=200)


rdsfile <- readRDS("D:/FLPS/results/gpcm_1000_100_0.2_0.5_TRUE_n_10.rds")

apply(rdsfile$sdat$lv.resp[1:500,c(1,64,91)], 2, table)

apply(rdsfile$sdat$lv.resp[1:500,], 1, table)


summary(rdsfile$fit, pars = "tau")[[1]][1:10,]
rdsfile$sdat$lv.par[1:10,]


a1 %>%
  filter(str_detect(cond, "1000_50")) %>%
  group_by(cond, twhich) %>%
  summarise(mean(CIin))

add_poly <- readRDS("D:/FLPS/results_poly/gpcm_2000_100_0.2_0.5_TRUE_n_220815001.rds")

apply(add_poly$sdat$lv.resp[1:500,c(1:3)], 2, table)



# ---------------------------------------------
apply(rdsfile$sdat$lv.resp[1:500,c(1,64,91)], 2, table)
apply(add_poly$sdat$lv.resp[1:500,c(1:3)], 2, table)

a1 %>%
  filter(str_detect(cond, "1000_50")) %>%
  group_by(twhich) %>%
  summarise(mean(CIin))

# ---------------------------------------------


gpcm_1 %>%
  filter(str_detect(cond, "1000_200")) %>%
  group_by(twhich) %>%
  summarise(
    mean(CIin),
    mean(err)
  )

gpcm_1 %>%
  filter(str_detect(cond, "1000_50")) %>%
  group_by(twhich) %>%
  summarise(
    mean(CIin),
    mean(err)
    )

gpcm_1 %>%
  filter(str_detect(cond, "500_100")) %>%
  group_by(twhich) %>%
  summarise(
    mean(CIin),
    mean(err)
  )

gpcm_1 %>%
  filter(str_detect(cond, "1000_100")) %>%
  group_by(twhich) %>%
  summarise(
    mean(CIin),
    mean(err)
  )

gpcm_1 %>%
  # filter(Rhat >= 1.1) %>%
  filter(str_detect(cond, "2000_100")) %>%
  group_by(twhich) %>%
  summarise(
    mean(CIin),
    mean(err)
  )



gpcm_1 %>%
  filter(str_detect(cond, "2000_100")) %>%
  filter(prior == "15") %>%
  filter(CIin == 0)


a1 %>%
  # select(cond, c(1,2,3,4,6, 7, 11,15,  17))
  mutate(
    cond = str_remove(cond, "_0.2_0.5_TRUE_n")
  ) %>%
  filter(str_detect(cond, "2000_100")) %>%
  select(cond, c(1,2,3,4,6, 7, 11,15,  17))

target <- fs::dir_ls("D:/FLPS/results_poly", regexp = "220820")

target<- fs::dir_ls("D:/FLPS/results_poly", regexp = "grm_2000_100_0.2_0.5_TRUE_n_220815001")

noncov_rep <- readRDS(target[1])

apply(noncov_rep$sdat$lv.resp[1:1000,c(1:3)], 2, table)

library(mirt)
fit <- mirt(data = noncov_rep$sdat$lv.resp,
            model = 1, itemtype = "gpcm")
mirt1 <- coef(fit, simplify = T, IRTpars = T)$items

true_ipar <- noncov_rep$sdat$lvinfo$ipar

rstan1 <- summary(noncov_rep$fit, pars = "tau")
rstan1 <- matrix(rstan1$summary[,1], ncol = 3, byrow = T)

true_ipar[,2]
mirt1[,2]
rstan1[,1]

