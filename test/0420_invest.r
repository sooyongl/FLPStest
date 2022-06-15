library(tidyverse); library(ggforce)

bayes_res <- readRDS("results/cleaned/0420_res_extracted_cleaned.rds")

res <- bayes_res

res_struct <- res %>%
  filter(str_detect(par_name, "^a|^b")) %>%
  select(par_name, err, cond) %>%
  group_by(par_name, cond) %>%
  summarise(err = mean(err)) %>%
  # filter(str_detect(cond, "uniftightaddprior")) %>%
  # slice(1366)
  spread("par_name","err")

eta <- res %>%
  filter(str_detect(par_name, "eta")) %>%
  mutate(par_name = "eta") %>%
  group_by(cond) %>%
  summarise(eta = mean(err)) %>%
  mutate(par_name = "eta")

# eta %>% filter(str_detect(cond, "rasch"))

lambda <- res %>%
  filter(str_detect(par_name, "lambda")) %>%
  group_by(cond, par_name) %>%
  summarise(lambda = mean(err)) %>%
  mutate(par_name = "lambda")

tau <- res %>%
  filter(str_detect(par_name, "tau")) %>%
  group_by(cond, par_name) %>%
  summarise(tau = mean(err)) %>%
  mutate(par_name = "tau")


# structural parameters -------------------------------------------
res_all <-
  res_struct %>%
  gather("par_name","value",-cond) %>%

  bind_rows(
    eta %>% select(cond, par_name, value=eta),
    lambda %>% select(cond, par_name, value=lambda),
    tau %>% select(cond, par_name, value=tau)
  ) %>%
  separate(cond, c("estimator","samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  mutate(
    samplesize = factor(samplesize, levels = c("100", "2000")),
    nitem = factor(nitem, levels = c("30")),
    lvmodel = factor(lvmodel, levels = c("2pl"))
  ) %>%
  mutate(
    par_name = factor(par_name,
                      levels = c("b00","b0", "b11" ,"a11" ,"bu11","bu12",
                                 "by1", "by2","lambda","tau","eta"))
  )

unique(res_all$estimator)
saveRDS(res_all, "test/shinydata/0420_data.rds")


# measurement part -------------------------
mpart <- res %>%
  filter(str_detect(par_name, "eta|lambda|tau"))

saveRDS(mpart, "test/shinydata/0420_mpart.rds")

estimator1 = "diffuse"
samplesize = "2000"
repn = "11"
condname <- paste(estimator1, samplesize, "30_2pl_TRUE_n", repn, sep = "_")

mpart_sub <- mpart %>%
  filter(cond == condname) %>%
  mutate(
    par_name =
      case_when(str_detect(par_name, "eta") ~ "eta",
                str_detect(par_name, "lambda") ~ "lambda",
                TRUE ~ "tau")
  )

correlations <- mpart_sub %>%
  group_by(par_name) %>%
  summarise(corr = cor(true_param, est_mean))

means <- mpart_sub %>%
  group_by(par_name) %>%
  summarise(
    var_est  = var(est_mean),
    var_true  = var(true_param),
    bias     = mean(est_mean-true_param),
    sdbias   = sd(est_mean-true_param)
  )


peta <- mpart_sub %>%
  filter(par_name == "eta") %>%
  mutate(group = rep(c("trt","ctl"), each = nrow(.)/2)) %>%
  ggplot() +
  geom_point(aes(x = true_param, y = est_mean, color = group)) +
  geom_abline(slope = 1, intercept = 0) +
  annotate(geom = "text",
           x = -Inf, y = Inf,
           size = 6,
           vjust = 1,
           hjust = -2,
           label = paste0("Cor = ",
                          round(correlations$corr[1],3))) +
  annotate(geom = "text",
           x = -Inf, y = Inf,
           size = 6,
           vjust = 2,
           hjust = -2,
           label = paste0("Bias = ",
                          round(means$bias[1],3))) +
  annotate(geom = "text",
           x = -Inf, y = -Inf,
           size = 6,
           vjust = -2,
           hjust = -2,
           label = paste0("Variance of eta est = ",
                          round(means$var_est[1],3))) +
  annotate(geom = "text",
           x = -Inf, y = -Inf,
           size = 6,
           vjust = -1,
           hjust = -2,
           label = paste0("Variance of true = ",
                          round(means$var_true[1],3)))


plam <- mpart_sub %>%
  filter(par_name == "lambda") %>%
  ggplot() +
  geom_point(aes(x = true_param, y = est_mean)) +
  geom_abline(slope = 1, intercept = 0) +
  annotate(geom = "text",
           x = -Inf, y = Inf,
           size = 6,
           vjust = 1,
           hjust = -2,
           label = paste0("Cor = ",
                          round(correlations$corr[2],3))) +
  annotate(geom = "text",
           x = -Inf, y = Inf,
           size = 6,
           vjust = 2,
           hjust = -2,
           label = paste0("Bias = ",
                          round(means$bias[2],3)))


ptau <- mpart_sub %>%
  filter(par_name == "tau") %>%
  ggplot() +
  geom_point(aes(x = true_param, y = est_mean)) +
  geom_abline(slope = 1, intercept = 0) +
  annotate(geom = "text",
           x = -Inf, y = Inf,
           size = 6,
           vjust = 1,
           hjust = -2,
           label = paste0("Cor = ",
                          round(correlations$corr[3],3))) +
  annotate(geom = "text",
           x = -Inf, y = Inf,
           size = 6,
           vjust = 2,
           hjust = -2,
           label = paste0("Bias = ",
                          round(means$bias[3],3)))


ggpubr::ggarrange(peta, plam, ptau,
                  nrow = 3,
                  labels = c("Eta","Lambda","Tau"))


# res_all <- readRDS("test/shinydata/0420_data.rds")



