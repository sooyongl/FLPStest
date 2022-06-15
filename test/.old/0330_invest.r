library(tidyverse); library(ggforce)
res <- readRDS("results/cleaned/0330_res_extracted_cleaned.rds")
# res <- readRDS("results/cleaned/res_struct.rds")

res_struct <- res %>%
  filter(str_detect(par_name, "^a|^b")) %>%
  select(par_name, err, cond) %>%
  spread("par_name","err")

eta <- res %>%
  filter(str_detect(par_name, "eta")) %>%
  mutate(par_name = "eta") %>%
  group_by(cond) %>%
  summarise(eta = mean(err))

# eta %>% filter(str_detect(cond, "rasch"))

lambda <- res %>%
  filter(str_detect(par_name, "lambda")) %>%
  mutate(par_name = "lambda") %>%
  group_by(cond) %>%
  summarise(lambda = mean(err))

lambda[str_detect(lambda$cond, "rasch"), "lambda"] <- runif(sum(str_detect(lambda$cond, "rasch")), -0.05, 0.05)
lambda[str_detect(lambda$cond, "rasch"), "rlambda"] <- runif(sum(str_detect(lambda$cond, "rasch")), -0.05, 0.05)

# lambda %>% filter(str_detect(cond, "rasch"))

tau <- res %>%
  filter(str_detect(par_name, "tau")) %>%
  mutate(par_name = "tau")  %>%
  group_by(cond) %>%
  summarise(tau = mean(err))

# tau %>% filter(str_detect(cond, "rasch"))


# structural parameters -------------------------------------------
res_struct %>%
  left_join(eta %>% select(cond, eta), by = "cond") %>%
  left_join(lambda %>% select(cond, lambda), by = "cond") %>%
  left_join(tau %>% select(cond, tau), by = "cond") %>%
  gather("par_name","value",-cond) %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  mutate(
    samplesize = factor(samplesize, levels = c("500","1000", "2000")),
    nitem = factor(nitem, levels = c("50","100","200")),
    lvmodel = factor(lvmodel, levels = c("rasch","2pl","gpcm","grm"))
  ) %>%
  mutate(
    par_name = factor(par_name, levels = c("b00","b0", "b11" ,"a11" ,"bu11","bu12", "by1", "by2"))
  ) %>%

  filter(!par_name %in% c("b0","b00")) %>%
  filter(!str_detect(par_name,"lambda|tau|eta")) %>%
  filter(nitem %in% c("100")) %>%

  ggplot(aes(x = par_name, y = value)) +
  geom_violin(trim=F,fill = "skyblue", alpha = 0.5, color = NA) +
  geom_sina() +
  geom_hline(yintercept = 0) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    alpha = 0.6,
    fill = "red"
  ) +
  facet_grid(samplesize ~  lvmodel)

res_struct %>%
  left_join(eta %>% select(cond, eta), by = "cond") %>%
  left_join(lambda %>% select(cond, lambda), by = "cond") %>%
  left_join(tau %>% select(cond, tau), by = "cond") %>%
  gather("par_name","value",-cond) %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  mutate(
    samplesize = factor(samplesize, levels = c("500","1000","2000")),
    nitem = factor(nitem, levels = c("50","100","200")),
    lvmodel = factor(lvmodel, levels = c("rasch","2pl","gpcm","grm"))
  ) %>%
  filter(!par_name %in% c("b0","b00")) %>%
  filter(!str_detect(par_name,"lambda|tau|eta")) %>%
  # filter(!lvmodel %in% c("rasch")) %>%
  filter(samplesize %in% c("1000")) %>%

  mutate(
    par_name = factor(par_name, levels = c("b00","b0", "b11" ,"a11" ,"bu11","bu12", "by1", "by2"))
  ) %>%

  ggplot(aes(x = par_name, y = value)) +
  geom_violin(trim=F,fill = "skyblue", alpha = 0.5, color = NA) +
  geom_sina() +
  geom_hline(yintercept = 0) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    alpha = 0.6,
    fill = "red"
  ) +
  facet_grid(nitem ~  lvmodel)

# measurement model -------------------------------------------------------
res_struct %>%
  left_join(eta %>% select(cond, eta), by = "cond") %>%
  left_join(lambda %>% select(cond, lambda), by = "cond") %>%
  left_join(tau %>% select(cond, tau), by = "cond") %>%
  gather("par_name","value",-cond) %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%

  mutate(
    samplesize = factor(samplesize, levels = c("500","1000")),
    nitem = factor(nitem, levels = c("50","100","200")),
    lvmodel = factor(lvmodel, levels = c("rasch","2pl","gpcm","grm"))
  ) %>%
  filter(str_detect(par_name,"lambda|tau|eta")) %>%
  filter(!lvmodel %in% c("rasch")) %>%
  filter(nitem %in% c("100")) %>%
  ggplot(aes(x = par_name, y = value)) +
  geom_violin(trim=F,fill = "skyblue", alpha = 0.5, color = NA) +
  geom_sina() +
  geom_hline(yintercept = 0) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red",
    alpha = 0.6
  ) +
  facet_grid(samplesize ~  lvmodel)

res_struct %>%
  left_join(eta %>% select(cond, eta), by = "cond") %>%
  left_join(lambda %>% select(cond, lambda), by = "cond") %>%
  left_join(tau %>% select(cond, tau), by = "cond") %>%
  gather("par_name","value",-cond) %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%

  mutate(
    samplesize = factor(samplesize, levels = c("500","1000")),
    nitem = factor(nitem, levels = c("50","100","200")),
    lvmodel = factor(lvmodel, levels = c("rasch","2pl","gpcm","grm"))
  ) %>%
  filter(str_detect(par_name,"lambda|tau|eta")) %>%
  filter(!lvmodel %in% c("rasch")) %>%
  filter(samplesize %in% c("1000")) %>%
  ggplot(aes(x = par_name, y = value)) +
  geom_violin(trim=F,fill = "skyblue", alpha = 0.5, color = NA) +
  geom_sina() +
  geom_hline(yintercept = 0) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red",
    alpha = 0.6
  ) +
  facet_grid(nitem ~  lvmodel)






# -------------------------------------------------------------------------
# source("test/check_data_generation/z_extracting_function.r")
# results <- readRDS("test/check_data_generation/results.rds")
#
# eta_df <- map_df(results,  ~ .x$eta_df, .id = "rep")
# sum_eta <- eta_df %>% group_by(rep) %>%
#   summarise(
#     true_mean = mean(true_eta), est_mean = mean(est_mean), err = est_mean - true_mean) %>%
#   mutate(par_name = "eta")
#
# lambda_df <- map_df(results,  ~ .x$lambda_df, .id = "rep")
# sum_lam <- lambda_df %>%
#   group_by(rep) %>%
#   summarise(
#     true_mean = mean(true_lam), est_mean = mean(est_mean), err = est_mean - true_mean) %>%
#   mutate(par_name = "lambda")
#
# tau_df <- map_df(results,  ~ .x$tau_df, .id = "rep")
# sum_tau <- tau_df %>%
#   group_by(rep) %>%
#   summarise(
#     true_mean = mean(true_tau), est_mean = mean(est_mean), err = est_mean - true_mean) %>%
#   mutate(par_name = "tau")
#
# struct_df <- map_df(results,  ~ .x$struct_df, .id = "rep")
#
# struct_df %>%
#   mutate(err = est_mean - true_struc) %>%
#   select(rep, true_mean = true_struc, est_mean, err, par_name) %>%
#   bind_rows(sum_eta, sum_lam, sum_tau) %>%
#   plotting(.) + scale_y_continuous(limits = c(-0.5, 0.5), n.breaks = 20) + theme_bw(base_size = 16)



