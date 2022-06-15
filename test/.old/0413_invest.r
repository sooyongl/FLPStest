library(tidyverse); library(ggforce)

results <- readRDS("test/check_data_generation/results.rds")

cond_df <- map_chr(results,  ~ .x[[1]]) %>% tibble(.) %>%
  mutate(rep = row_number()) %>%
  mutate(rep = as.character(rep)) %>%
  set_names(c("condition", "rep"))

eta_df <- map_df(results,  ~ .x$eta_df, .id = "rep")
sum_eta <- eta_df %>%
  left_join(cond_df, by ="rep") %>%
  mutate(
    err = est_mean - true_eta,
    par_name = "eta") %>%
  rename(true_mean = "true_eta") %>%
  select(condition, par_name, err, true_mean, est_mean)


lambda_df <- map_df(results,  ~ .x$lambda_df, .id = "rep")
sum_lam <- lambda_df %>%
  left_join(cond_df, by ="rep") %>%
  mutate(
    err = est_mean - true_lam,
    par_name = "lambda") %>%
  rename(true_mean = "true_lam") %>%
  select(condition, par_name, err, true_mean, est_mean)

tau_df <- map_df(results,  ~ .x$tau_df, .id = "rep")
sum_tau <- tau_df %>%
  left_join(cond_df, by ="rep") %>%
  mutate(
    err = est_mean - true_tau,
    par_name = "tau") %>%
  rename(true_mean = "true_tau") %>%
  select(condition, par_name, err, true_mean, est_mean)

struct_df <- map_df(results,  ~ .x$struct_df, .id = "rep")
struct_df <- struct_df %>%
  left_join(., cond_df, by = c("rep")) %>%
  mutate(err = est_mean - true_struc) %>%
  select(condition, par_name, err, true_mean = true_struc, est_mean)


total_res <-
  struct_df %>%
  bind_rows(sum_eta, sum_lam, sum_tau) %>%
  separate(condition, c("condition", "lvmodel","rep"), sep = "_") %>%
  mutate(
    condition = case_when(
      condition == "logn" ~ "logn_missing_4cov",
      condition == "normal" ~ "normal_missing_4cov",
      condition == "unif" ~ "unif_missing_4cov",

      condition == "lognzcomplefour" ~ "logn_complete_4cov",
      condition == "normalzcomplezfour" ~ "normal_complete_4cov",
      condition == "unifzcomplezfour" ~ "unif_complete_4cov",

      condition == "lognzcompletwoc" ~ "logn_complete_2cov",
      condition == "normalzcompleztwoc" ~ "normal_complete_2cov",
      condition == "unifzcompleztwoc" ~ "unif_complete_2cov",

      condition == "unifztwo" ~ "unif_missing_2cov",
      condition == "normalztwo" ~ "normal_missing_2cov",
      condition == "lognztwo" ~ "logn_missing_2cov",

      condition == "lognzperfectpriortwo" ~ "perflogn_missing_2cov",
      condition == "lognzperfectpriortwozcompl" ~ "perflogn_complete_2cov"

    )
  )
total_res <- readRDS("test/0406_total_res.rds")

total_res <- total_res %>%
  group_by(condition, par_name, rep) %>%
  summarise(err = mean(err))


total_res1 <- total_res %>%
  filter(condition == "logn_complete_2cov")

repn <- unique(total_res1$rep)

pick_repn <- repn[2]

total_res1 %>%
  mutate(hlt = case_when(
    rep == pick_repn ~ "picked",
    TRUE ~ "none"
  )) %>%
  ggplot(aes(x = par_name, y = err, alpha = hlt, size = hlt)) +
  geom_violin(
    trim=F,
    fill = "skyblue", alpha = 0.5, color = NA) +
  geom_point(position = position_jitter(width = 0.05)) +
  # ggforce::geom_sina(size = 2) +
  geom_hline(yintercept = 0) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 2,
    shape = 24,
    alpha = 0.8,
    fill = "red"
  ) +
  facet_wrap(. ~ condition) +
  scale_alpha_manual(values = c(0.2, 1)) +
  scale_size_manual(values = c(1, 5)) +
  scale_y_continuous(n.breaks = 10)



# individual factor score estimates (eta) ---------------------------------------
total_res %>%
  filter(par_name == "eta") %>%
  filter(condition %in% c("logn_complete_4cov","normal_complete_4cov")) %>%
  ggplot() +
  geom_point(aes(true_mean, est_mean), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(rep~condition)

# factor loading (lambda) ---------------------------------------
total_res %>%
  filter(par_name == "lambda") %>%
  filter(condition %in% c("logn_complete_4cov")) %>%
  ggplot() +
  geom_point(aes(true_mean, est_mean), alpha = 1) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(rep~condition)


# intercept (difficulty) (tau) ---------------------------------------
total_res %>%
  filter(par_name == "tau") %>%
  filter(condition %in% c("logn_complete_4cov")) %>%
  ggplot() +
  geom_point(aes(true_mean, est_mean), alpha = 1) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(rep~condition)

###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
res <- readRDS("results/cleaned/0406_res_extracted_cleaned.rds")
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
res_all <- res_struct %>%
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
    par_name = factor(par_name,
                      levels = c("b00","b0", "b11" ,"a11" ,"bu11","bu12",
                                 "by1", "by2","lambda","tau","eta"))
  ) %>%

  # filter(!par_name %in% c("b0","b00")) %>%
  # filter(!str_detect(par_name,"lambda|tau|eta")) %>%
  filter(nitem %in% c("100"))


res_all %>%
  filter(
    samplesize == "1000",
    nitem == "100"
  ) %>%
  mutate(hlt = case_when(
    rep == "1" ~ "picked",
    TRUE ~ "none"
  )) %>%
  ggplot(aes(x = par_name, y = value, alpha = hlt, size = hlt)) +
  geom_violin(
    trim = F,
    fill = "skyblue", alpha = 0.5, color = NA) +
  geom_point(position = position_jitter(width = 0.05)) +
  geom_hline(yintercept = 0) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 2,
    shape = 24,
    alpha = 0.8,
    fill = "red"
  ) +
  scale_alpha_manual(values = c(0.2, 1)) +
  scale_size_manual(values = c(1, 5)) +
  scale_y_continuous(n.breaks = 10)
