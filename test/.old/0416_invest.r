library(tidyverse); library(ggforce)

bayes_res <- readRDS("results/cleaned/0416_res_extracted_cleaned.rds")
mplus_res <- readRDS("test/test_mplus/0416_mplus_res.rds")

res <- bayes_res

res_struct <- res %>%
  filter(str_detect(par_name, "^a|^b")) %>%
  select(par_name, err, cond) %>%
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


# mplus -------------------------------------------------------------------
names(mplus_res)
res <- mplus_res %>%
  separate(condition, c("estimator","samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  select(-paramHeader) %>%
  mutate(
    param = case_when(str_detect(param, "tau") ~ "tau",
                      str_detect(param, "lambda") ~ "lambda",
                      TRUE ~ param
                      ),
    est = case_when(str_detect(est, "\\*") ~ "99999",
                    TRUE ~ est),

    est = as.numeric(est),
    err = est - true_param
  ) %>%
  rename("par_name" = "param", "value" = "err") %>%
  tibble() %>%
  select(estimator, samplesize, nitem, lvmodel, a, b, rep, par_name, value)

# res %>% filter(par_name == "lambda")
# res_all %>% filter(par_name == "lambda")

saveRDS(bind_rows(res,res_all), "test/shinydata/0416_data.rds")

