library(tidyverse); library(ggforce)

data_idx <- "0529"
bayes_res <- readRDS(paste0("results/cleaned/",data_idx,"_extracted_cleaned.rds"))

nrow(bayes_res)

bayes_res <- bayes_res %>%
  mutate(rerr =
           (est_mean - true_param)/true_param,
         .before = cond)

bayes_res <- bayes_res %>%
  mutate(par_name = case_when(
    par_name == "betaU[1]" ~ "bu11",
    par_name == "betaU[2]" ~ "bu12",
    par_name == "betaY[1]" ~ "by1",
    par_name == "betaY[2]" ~ "by2",
    par_name == "a1" ~ "a11",
    par_name == "b1" ~ "b11",
    TRUE ~ par_name
    )
  )

res <- bayes_res

res_struct <- res %>%
  filter(str_detect(par_name, "^a|^b")) %>%
  select(par_name, err, rerr, cond) %>%
  group_by(par_name, cond) %>%
  summarise(err = mean(err),
            rerr = mean(rerr))
  # filter(str_detect(cond, "uniftightaddprior")) %>%
  # slice(1366)
  # pivot_wider(names_from = par_name, values_from = c(err, rerr))
  # spread("par_name","err")

eta <- res %>%
  filter(str_detect(par_name, "eta")) %>%
  mutate(par_name = "eta") %>%
  group_by(cond) %>%
  summarise(
    eta = mean(err),
    reta = mean(rerr)) %>%
  mutate(par_name = "eta")

# eta %>% filter(str_detect(cond, "rasch"))

lambda <- res %>%
  filter(str_detect(par_name, "lambda")) %>%
  group_by(cond, par_name) %>%
  summarise(lambda = mean(err),
            rlambda = mean(rerr)) %>%
  mutate(par_name = "lambda")

tau <- res %>%
  filter(str_detect(par_name, "tau")) %>%
  group_by(cond, par_name) %>%
  summarise(tau = mean(err),
            rtau = mean(rerr)) %>%
  mutate(par_name = "tau")


# structural parameters -------------------------------------------
# res_all %>% distinct(lvmodel)
res_all <-
  res_struct %>%
  rename(value = err, rvalue = rerr) %>%
  # gather("par_name","value",-cond) %>%

  bind_rows(
    eta %>% select(cond, par_name, value=eta, rvalue = reta),
    lambda %>% select(cond, par_name, value=lambda, rvalue = rlambda),
    tau %>% select(cond, par_name, value=tau, rvalue=rtau)
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
                     levels = c("rasch","2pl","grm","gpcm","2plunif","2plnormal","2pllogn"))
  ) %>%
  mutate(
    par_name = factor(par_name,
                      levels = c("b00","b0", "b11" ,"a11" ,"bu11","bu12",
                                 "by1", "by2","lambda","tau","eta"))
  )

saveRDS(res_all, paste0("test/shinydata/", data_idx, "_data.rds"))

res_all %>% distinct(lvmodel)


res_all <- readRDS(paste0("test/shinydata/", data_idx, "_data.rds"))



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

mpart <- mpart %>%
  mutate(
    est_mean =
      case_when(
        str_detect(par_name, "tau") &
          str_detect(cond, "rasch") ~ -est_mean,
        TRUE ~ est_mean
      )
  )

saveRDS(mpart, paste0("test/shinydata/", data_idx, "_mpart.rds"))

mpart <- readRDS(paste0("test/shinydata/", data_idx, "_mpart.rds"))

a1 <- mpart %>%
  filter(str_detect(par_name,"lambda") ) %>%
  # filter(lvmodel != "rasch") %>%
  separate(cond,
           c("lvmodel","samplesize","nitem","r2y","r2eta","linearity","outdist","rep"), "_") %>%
  mutate(
    overest = err > 0,
    underest = err < 0,
    evenest  = err == 0
  )

a1

a1 %>%
  group_by(lvmodel, samplesize, nitem) %>%
  summarise(
    mean_overest = mean(overest),
    mean_underest = mean(underest),
    mean_evenest = mean(evenest)
  )



lvmodel = "2pl"
samplesize = "1000"
nitem = "100"
repn = "1"
condname <- paste(lvmodel, samplesize, nitem,"0.2_0.5_TRUE_n", repn, sep = "_")

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



