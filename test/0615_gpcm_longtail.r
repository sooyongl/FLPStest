library(tidyverse)
library(rstan)
library(ggpubr)

library(coda)

# https://go.documentation.sas.com/doc/en/pgmsascdc/9.4_3.3/statug/statug_introbayes_sect025.htm#statug.introbayes.bayesvisual
# https://cran.r-project.org/web/packages/JointAI/vignettes/AfterFitting.html

data_idx <- "0608"

# res_all <- readRDS(paste0(base_path,"/","test/shinydata/", data_idx, "_data.rds"))
mpart <- readRDS(paste0("test/shinydata/", data_idx, "_mpart.rds"))
mpart$cond[[1]]
a1 <- mpart %>%
  filter(str_detect(par_name,"lambda") ) %>%
  # filter(lvmodel != "rasch") %>%
  separate(cond,
           c("lvmodel","samplesize","nitem","r2y","r2eta","linearity","outdist","rep"), "_") %>%
  mutate(
    overest = err > 0,
    underest = err < 0,
    evenest  = err == 0
  ) %>%
  filter(!str_detect(lvmodel, "normal|logn|unif"))

get_fit <- function(filtered, i, inc_warmup = T) {

  lvmodel <- filtered[i, "lvmodel"] %>% unlist()
  samplesize <- filtered[i, "samplesize"] %>% unlist()
  nitem      <- filtered[i, "nitem"] %>% unlist()
  rep        <- filtered[i, "rep"] %>% unlist()

  bias        <- filtered[i, "err"] %>% unlist()
  par_name        <- filtered[i, "par_name"] %>% unlist()

  num_par <- str_split(par_name, "lambda\\[")[[1]][2]
  num_par <- str_split(num_par, ",")[[1]][1]
  num_par <- as.numeric(num_par)

  outlier <- readRDS(
    paste0("D:/FLPS/results/",lvmodel, "_", samplesize,"_", nitem, "_0.2_0.5_TRUE_n_",rep, ".rds"))

  sfit <- outlier$fit

  est_v <- summary(sfit, pars = par_name)
  true_v <- outlier$sdat$lv.par$a1[num_par]

  p <- stan_trace(sfit, pars = par_name, inc_warmup = inc_warmup)


  mcmc_object <- As.mcmc.list(sfit, pars = par_name)

  # effectiveSize(mcmc_object) # The effective sample size (ESS) measures the amount by which autocorrelation in samples increases uncertainty (standard errors) relative to an independent sample
  gew <- geweke.diag(mcmc_object,frac1=.1,frac2=.5);
  hei <- heidel.diag(mcmc_object);
  gel <- gelman.diag(mcmc_object)

  autocr <-  autocorr.diag(mcmc_object, relative=T)

  list(
    cond = paste0(lvmodel, "_", samplesize,"_", nitem,"_",rep),
    bias = bias,
    est_v = est_v$summary, true_v = true_v, p = p,
    modelconver =
      list(gew = gew, hei = hei, gel = gel, autocr = autocr)
    )
}

# invest_ss <- "500"
# invest_ni <- "100"
# invest_lvm <- "gpcm"

outliers <- a1 %>%
  filter(err != 0) %>%
  # filter(lvmodel == invest_lvm) %>%
  # filter(samplesize == invest_ss, nitem == invest_ni) %>%
  select(lvmodel, samplesize, nitem, rep, par_name, err) %>%
  arrange(-abs(err)) %>%
  slice(1:10)

outliers %>% print(n = 100)

for(i in 1:10) {
  print(i)
  outlier_p <- get_fit(outliers, i)
  saveRDS(outlier_p, paste0("test/outlier_check/outlier_",i))
}

# ggarrange(outlier_p1$p, outlier_p2$p, ncol = 1)

# near 0 error ------------------------------------------------------------
set.seed(1)
unbiased <- a1 %>%
  filter(err != 0) %>%
  select(lvmodel, samplesize, nitem, rep, par_name, err) %>%
  filter(abs(err) < 0.1 ) %>%
  sample_n(10)

for(i in 1:10) {
  print(i)
  unbiased_p <- get_fit(unbiased, i)
  saveRDS(unbiased_p, paste0("test/outlier_check/unbiased_",i))
}

# -------------------------------------------------------------------------
set.seed(1)
middle <- a1 %>%
  filter(err != 0) %>%
  select(lvmodel, samplesize, nitem, rep, par_name, err) %>%
  filter(abs(err) < 0.5 & abs(err) > 0.1 ) %>%
  sample_n(10)

for(i in 1:10) {
  print(i)
  middle_p <- get_fit(middle, i)
  saveRDS(middle_p, paste0("test/outlier_check/middle_",i))
}



