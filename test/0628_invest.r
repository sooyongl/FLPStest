library(tidyverse); library(ggforce); library(foreach)

# cleaned <- fs::dir_ls("D:/FLPS/results/cleaned", regexp = "cleaned_")
# length(cleaned)
# # cleaned <- str_subset(cleaned, "unif|normal")
#
# combined <- foreach(i = 1:length(cleaned)) %do% {
#   a1 <- readRDS(cleaned[i])
#   a1$stan_res
# } %>%
#   bind_rows()
#
# rownames(combined) <- NULL
#
# combined <- combined %>% rename(true_param = true_parm)
#
data_idx <- "0628"
# saveRDS(combined, paste0("results/cleaned/",data_idx,"_extracted_cleaned.rds"))

bayes_res <- readRDS(paste0("results/cleaned/",data_idx,"_extracted_cleaned.rds"))
bayes_res <- bayes_res %>% rename(true_param = true_parm)

nrow(bayes_res)

bayes_res <- bayes_res %>% mutate(rerr = (err)/true_param, .before = cond)

bayes_res <- bayes_res %>% tibble()

bayes_res <- bayes_res %>%
  mutate_if(is.factor, as.character)

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



res <- bayes_res

res_struct <- res %>%
  filter(str_detect(par_name, "^a|^b")) %>%
  select(par_name, err, rerr, sd, X2.5., X97.5., Rhat, CIin, cond) %>%
  group_by(par_name, cond) %>%
  summarise(err = mean(err),
            rerr = mean(rerr),
            Rhat = mean(Rhat),

            X2.5. = mean(X2.5.),
            X97.5. = mean(X97.5.),

            coverage = mean(CIin)
  )

eta <- res %>%
  filter(str_detect(par_name, "eta")) %>%
  mutate(par_name = "eta") %>%
  group_by(cond) %>%
  summarise(eta = mean(err),
            reta = mean(rerr),
            Rhat = mean(Rhat),

            X2.5. = mean(X2.5.),
            X97.5. = mean(X97.5.),

            coverage = mean(CIin)
  ) %>%
  mutate(par_name = "eta")

lambda <- res %>%
  filter(str_detect(par_name, "lambda")) %>%
  group_by(cond, par_name) %>%
  summarise(lambda = mean(err),
            rlambda = mean(rerr),
            Rhat = mean(Rhat),

            X2.5. = mean(X2.5.),
            X97.5. = mean(X97.5.),

            coverage = mean(CIin)
  ) %>%
  mutate(par_name = "lambda")

tau <- res %>%
  filter(str_detect(par_name, "tau")) %>%
  group_by(cond, par_name) %>%
  summarise(tau = mean(err),
            rtau = mean(rerr),
            Rhat = mean(Rhat),

            X2.5. = mean(X2.5.),
            X97.5. = mean(X97.5.),

            coverage = mean(CIin)
  ) %>%
  mutate(par_name = "tau")

# structural parameters -------------------------------------------
# res_all %>% distinct(lvmodel)
res_all <-
  res_struct %>%
  rename(value = err, rvalue = rerr) %>%
  # gather("par_name","value",-cond) %>%

  bind_rows(
    eta %>% select(cond, par_name, value=eta, rvalue = reta,
                   X2.5., X97.5., coverage, Rhat),
    lambda %>% select(cond, par_name, value=lambda, rvalue = rlambda,
                      X2.5., X97.5., coverage, Rhat),
    tau %>% select(cond, par_name, value=tau, rvalue=rtau,
                   X2.5., X97.5., coverage, Rhat)
  ) %>%
  separate(cond,
           c("lvmodel","samplesize","nitem","r2y","r2eta","linearity","outdist","rep"), "_") %>%
  mutate(
    samplesize =
      factor(samplesize, levels = c("500", "1000","2000")),
    nitem =
      factor(nitem, levels = c("50","100","200")),
    lvmodel =
      factor(lvmodel,
             levels = c("rasch","2pl","grm","gpcm",
                        "2plunif","2plnormal","2pllogn"))
  ) %>%
  mutate(
    par_name =
      factor(par_name,
             levels = c("b00","b0", "b11" ,"a11" ,"bu11","bu12",
                        "by1", "by2","lambda","tau","eta"))
  )

saveRDS(res_all, paste0("test/shinydata/", data_idx, "_data.rds"))


total_res <- res_all

# measurement part -------------------------
mpart <- res %>% filter(str_detect(par_name, "eta|lambda|tau"))

saveRDS(mpart, paste0("test/shinydata/", data_idx, "_mpart.rds"))
