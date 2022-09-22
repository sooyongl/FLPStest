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
# data_idx <- "0608"
# saveRDS(combined, paste0("results/cleaned/",data_idx,"_extracted_cleaned.rds"))

bayes_res <- readRDS(paste0("results/cleaned/",data_idx,"_extracted_cleaned.rds"))

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
  # filter(str_detect(cond, "uniftightaddprior")) %>%
  # slice(1366)
  # pivot_wider(names_from = par_name, values_from = c(err, rerr))
  # spread("par_name","err")

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

# eta %>% filter(str_detect(cond, "rasch"))

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


# res %>%
#   filter(str_detect(par_name, "tau")) %>%
#   filter(str_detect(cond, "grm")) %>%
#   print(n = 200)


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

# res_all %>% distinct(lvmodel)

# res_all <- readRDS(paste0("test/shinydata/", data_idx, "_data.rds"))


total_res <- res_all
n1 <- total_res %>%
  filter(par_name == "a11") %>%
  group_by(nsample=samplesize, nitem,lvmodel, linearity, ydist=outdist) %>%
  count() %>%
  mutate(additional = 100 - n)
n1 %>% print(n=100)

n2 <- n1 %>%
  ungroup() %>%
  mutate(filename = paste(nsample, nitem, lvmodel, linearity, ydist, sep = "_")) %>%
  filter(additional > 0 ) %>%
  select(filename, additional) %>%
  data.frame()

additional_rep <- c()
for(i in 1:nrow(n2)) {
  additional_rep <- c(additional_rep,
                      paste0(n2[i, "filename"], "_",
                             1:n2[i, "additional"]))
}
cond_table <- additional_rep %>%
  tibble(filename = .) %>%
  separate(filename,
           c("nsample","nitem","lvmodel","linearity","ydist", "rep"), "_") %>%
  mutate(
    nsample     = as.numeric(nsample),
    nitem       = as.numeric(nitem),
    linearity   = as.logical(linearity),
    rep         = as.numeric(rep) + 99900,
  ) %>%
  data.frame()
# saveRDS(cond_table, "test/additional_condition.rds")

# total_res %>%
#   filter(lvmodel == "rasch") %>%
#   filter(par_name %in% c("a11","b11")) %>%
#   mutate(rep = as.numeric(rep),
#          reptype = case_when(rep < 10 ~"1_10rep",
#                          rep > 100 ~ "over100rep",
#                          TRUE ~ "11_80rep")) %>%
#   group_by(
#     lvmodel, samplesize, nitem, par_name, reptype
#   ) %>%
#   summarise(mean(value)) %>%
#   print(n = 102)

# total_res %>%
#   filter(lvmodel == "rasch") %>%
#   filter(par_name %in% c("a11","b11")) %>%
#   ggplot(aes_string(x = "par_name", y = "value")) +
#   geom_violin(
#     trim=F,
#     fill = "skyblue", alpha = 0.5, color = NA) +
#   geom_sina(
#     position = position_jitter(width = 0.05),
#     alpha = 0.2) +
#
#   geom_hline(yintercept = 0)


# measurement part -------------------------
mpart <- res %>% filter(str_detect(par_name, "eta|lambda|tau"))

mpart %>% filter(str_detect(par_name, "tau") &
                   str_detect(cond, "rasch"))
saveRDS(mpart, paste0("test/shinydata/", data_idx, "_mpart.rds"))


# CI ----------------------------------------------------------------------

addCI <- res %>%
  select(cond, par_name, true_param, mean, sd, X2.5., X97.5., Rhat) %>%
  separate(cond,
           c("lvmodel","samplesize","nitem","r2y","r2eta","linearity","outdist","rep"), "_") %>%
  mutate(
    CIin = case_when(
      true_param > X2.5. & true_param < X97.5. ~ 1,
      TRUE ~ 0
    ),
    newpar_name = case_when(
      str_detect(par_name, "^eta") ~ "eta",
      str_detect(par_name, "^tau") ~ "tau",
      str_detect(par_name, "^lambda") ~ "lambda",
      TRUE ~ par_name
    )
  ) %>%
  filter(!is.nan(Rhat))

addCI %>%
  filter(lvmodel == "grm",
         samplesize == "1000",
         nitem == "50")

# a1 <- readRDS("D:/FLPS/results/rasch_1000_50_0.2_0.5_TRUE_n_100.rds")
# summary(a1$fit, pars = str_param)$summary


addCI %>%
  select(-r2y, -r2eta, -linearity, -outdist, -rep, -par_name) %>%
  group_by(lvmodel, samplesize, nitem, newpar_name) %>%
  summarise_all(mean) %>%
  filter(newpar_name == "tau") %>%
  # filter(str_detect(newpar_name, "[^lambda|tau|eta]")) %>%
  # filter(lvmodel == "rasch") %>%
  # filter(lvmodel == "2pl") %>%
  filter(lvmodel == "grm") %>%
  # filter(lvmodel == "gpcm") %>%
  print(n = 300)


