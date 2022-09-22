library(tidyverse)
bayes_res_0000 <- readRDS("test/paper_materials/bayes_res_0000.rds")

bayes_res_0000 <- bayes_res_0000 %>%
  rename(est_mean = mean) %>%
  separate(
    cond, c("lvmodel","samplesize","nitem","z1","z2","z3","z4","rep"),
    sep = "_"
  ) %>% select(-starts_with("z")) %>%
  filter(lvmodel %in% c("rasch","2pl","grm","gpcm"))

# unique(bayes_res_0000$par_name)[!str_detect(unique(bayes_res_0000$par_name), "^eta|^tau|^lambda")]

bayes_res_0001 <- pick_100 %>%
  left_join(bayes_res_0000,
            by = c("lvmodel","samplesize","nitem","rep")) %>%
  filter(lvmodel %in% c("rasch","2pl","grm","gpcm"))


# unique(bayes_res_0001$par_name)[!str_detect(unique(bayes_res_0001$par_name), "^eta|^tau|^lambda")]

if(FALSE) { # combine two bu and by
  bayes_res_0001 <- bayes_res_0001 %>%
    mutate(
      par_name = as.character(par_name),
      par_name = case_when(
        str_detect(par_name, "bu") ~ "bu",
        str_detect(par_name, "by") ~ "by",
        TRUE ~ par_name
      )) %>%
    group_by(lvmodel, samplesize, nitem, par_name, rep) %>%
    summarise(
      true_param = mean(true_param),
      est_mean = mean(est_mean),
      err = mean(err),
      X2.5. = mean(X2.5.),
      X97.5. = mean(X97.5.),
      Rhat = mean(Rhat)
    ) %>%
    mutate(
      CIin = case_when(
        true_param >= X2.5. & true_param <= X97.5. ~ 1,
        TRUE ~ 0
      )
    )
}

unique(bayes_res_0001$par_name)[!str_detect(unique(bayes_res_0001$par_name), "^eta|^tau|^lambda")]

bayes_res_0002 <- bayes_res_0001 %>%
  mutate(par_name1 = par_name) %>%
  separate(par_name1, c("par","num"), sep = "\\[") %>%
  mutate(
    num = str_remove(num, "\\]"),
    num = as.numeric(num),
    samplesize = as.numeric(samplesize),
    trt =
      case_when(
        str_detect(par, "eta") & num >= samplesize/2 ~ 1,
        str_detect(par, "eta") & num < samplesize/2 ~ 0,
        TRUE ~ NA_real_
      )
  ) %>%
  mutate(
    par_name0 = case_when(
      str_detect(par_name, "eta") ~ "eta",
      str_detect(par_name, "lambda") ~ "lambda",
      str_detect(par_name, "tau") ~ "tau",
      TRUE ~ par_name
    ),
    par_name0 = paste0(par_name0, trt),
    par_name0 = str_remove(par_name0, "NA")
  )

unique(bayes_res_0002$par_name)[!str_detect(unique(bayes_res_0002$par_name), "^eta|^tau|^lambda")]
unique(bayes_res_0002$par_name0)

saveRDS(bayes_res_0002, "test/paper_materials/bayes_res_0002.rds")

# saveRDS(bayes_res_0002, "test/paper_materials/bayes_res_0002_separate.rds")

bayes_res_0003 <-
  bayes_res_0002 %>%
  mutate(
    par_name0 =
      case_when(
        par_name0 == "a11" ~ "Omega",
        par_name0 == "b0" ~ "Tau0",
        par_name0 == "b1" ~ "Tau1",
        par_name0 == "bu" ~ "beta",
        par_name0 == "by" ~ "gamma",
        par_name0 == "tau" ~ "Intercept",
        par_name0 == "lambda" ~ "Slope",
        par_name0 == "eta0" ~ "eta_C",
        par_name0 == "eta1" ~ "eta_T",
        TRUE ~ par_name0
      )
  )

bayes_res_0003 <- bayes_res_0003 %>%
  select(lvmodel, samplesize, nitem, par_name, par_name0, rep, true_param, est_mean, X2.5., X97.5., Rhat, CIin) %>%
  mutate(
    nonc = if_else(Rhat > 1.1, 1, 0)
  )

bayes_res_0003 %>%
  filter(Rhat > 1.2) %>%
  print(n = 100)

bayes_res_0003 %>%
  filter(par_name0 == "bu11") %>%
  print(n = 100)

bayes_res_0003 %>%
  filter(par_name0 == "bu12") %>%
  print(n = 100)

bayes_res_0003 %>%
  group_by(lvmodel, samplesize, nitem, rep) %>%
  summarise(freq_nonc = sum(nonc)) %>%
  filter(freq_nonc > 0) %>%
  count(lvmodel, samplesize, nitem)

bayes_res_0003 %>%
  group_by(lvmodel, samplesize, nitem, par_name0, rep) %>%
  summarise(freq_nonc = sum(nonc)) %>%
  filter(freq_nonc > 0) %>%
  mutate(
    nitem = as.numeric(nitem),
    prop_nonc =
      case_when(
        str_detect(par_name0, "Slope|Intercept") ~ freq_nonc / (nitem),
        str_detect(par_name0, "eta_") ~ freq_nonc / (samplesize/2),
        TRUE ~ freq_nonc
      )
  ) %>%
  print(n = 200)

bayes_res_0003 %>%
  group_by(lvmodel, samplesize, nitem, par_name0) %>%
  summarise(freq_nonc = sum(nonc)) %>%
  filter(freq_nonc > 0) %>%
  print(n = 100)

bayes_res_0003 %>%
  # filter(str_detect(par_name0, "bu")) %>%
  group_by(lvmodel, par_name0) %>%
  summarise(freq_nonc = sum(nonc)) %>%
  filter(freq_nonc > 0) %>%
  print(n = 100)

bayes_res_0003 %>%
  # filter(str_detect(par_name0, "bu")) %>%
  group_by(lvmodel) %>%
  summarise(freq_nonc = sum(nonc)) %>%
  filter(freq_nonc > 0) %>%
  print(n = 100)

# ---------------------------------------------------------------
sddata <- mpart0 %>%
  filter(str_detect(par_name, "eta")) %>%
  filter(lvmodel %in% c("rasch","2pl","gpcm","grm")) %>%
  group_by(lvmodel, samplesize, nitem, rep) %>%
  summarise(
    est_m = mean(est_mean),
    est_var = sd(est_mean)
  )

mpart0 %>%
  filter(str_detect(par_name, "eta")) %>%
  filter(lvmodel %in% c("rasch","2pl","gpcm","grm")) %>%
  left_join(sddata, by = c("lvmodel","samplesize","nitem","rep")) %>%
  select(lvmodel, samplesize, nitem, rep, par_name, par_name0, true_param, est_mean, est_m, est_var) %>%
  mutate(
    outlier1 = abs(est_mean) > est_m + 1*est_var,
    outlier2 = abs(est_mean) > est_m + 2*est_var
  ) %>%
  group_by(lvmodel, samplesize, nitem) %>%
  summarise(
    mean(outlier1),
    mean(outlier2)
  ) %>%
  print(n=100)



overall <- mpart0 %>%
  filter(str_detect(par_name, "eta")) %>%
  filter(lvmodel %in% c("rasch","2pl","gpcm","grm")) %>%
  group_by(lvmodel, samplesize, nitem, rep) %>%
  summarise(
    true_mean = mean(true_param),
    est_mean0 = mean(est_mean),

    true_var = var(true_param),
    est_var = var(est_mean)
  )

overall %>%
  group_by(lvmodel, samplesize, nitem) %>%
  summarise(
    true_mean = mean(true_mean),
    est_mean0 = mean(est_mean0),

    true_var_m = mean(true_var),
    true_var_v = var(true_var),

    est_var_m = mean(est_var),
    est_var_v = var(est_var),
    # var_min  = min(est_var),
    # var_max  = max(est_var),
    q1 = quantile(est_var, probs = .25),
    q2 = quantile(est_var, probs = .5),
    q3 = quantile(est_var, probs = .75),
    q4 = quantile(est_var, probs = 1),
  ) %>%
  print(n=100)


overall %>%
  mutate(mean_err = est_mean0 - true_mean) %>%
  ggplot() +
  geom_histogram(aes(x = mean_err)) +
  # geom_histogram(aes(x = true_mean), color = "white") +
  # geom_histogram(aes(x = est_mean0)) +
  facet_grid(lvmodel ~ samplesize + nitem)


overall %>%
  mutate(var_err = est_var - true_var) %>%
  ggplot() +
  geom_histogram(aes(x = var_err)) +
  # geom_histogram(aes(x = true_mean), color = "white") +
  # geom_histogram(aes(x = est_mean)) +
  facet_grid(lvmodel ~ samplesize + nitem)

bytrt <- mpart0 %>%
  filter(str_detect(par_name, "eta")) %>%
  filter(lvmodel %in% c("rasch","2pl","gpcm","grm")) %>%
  group_by(lvmodel, samplesize, nitem, rep, par_name0) %>%
  summarise(
    true_mean = mean(true_param),
    est_mean0 = mean(est_mean),

    true_var = var(true_param),
    est_var = var(est_mean)
  )

bytrt %>%
  group_by(lvmodel,par_name0) %>%
  summarise(
    true_mean = mean(true_mean),
    est_mean0 = mean(est_mean0),

    true_var_m = mean(true_var),
    true_var_v = var(true_var),
    est_var_m = mean(est_var),
    est_var_v = var(est_var)
  ) %>%
  print(n=100)

bytrt %>%
  mutate(var_err = est_var - true_var) %>%
  ggplot() +
  geom_histogram(aes(x = var_err)) +
  # geom_histogram(aes(x = true_mean), color = "white") +
  # geom_histogram(aes(x = est_mean0)) +
  facet_grid(lvmodel ~ samplesize + nitem)


