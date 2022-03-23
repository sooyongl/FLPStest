# rm(list = ls())
library(rstan)
library(foreach)
library(doParallel)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(purrr)
library(Rmpi)

for(i in list.files("R", full.names = T, pattern = "r$")) source(i);rm(i)
source_funs <- ls()

# core setting ------------------------------------------------------------
# cl <- getMPIcluster()
# doParallel::registerDoParallel(cl)

# n_cores <- detectCores() - 2; print(n_cores)
# cl <- parallel::makeCluster(n_cores)
# doSNOW::registerDoSNOW(cl)

rds_name <- list.files("F:/FLPS/results", full.names = T)
# rds_name <- list.files("results", full.names = T)
res_list <- foreach(
  irds = 1:length(rds_name)#,
  # .packages = c("rstan","dplyr","stringr","purrr","magrittr","tidyr"),
  # .errorhandling = 'pass',
  # .export = source_funs
) %do% {

  # irds <- 1

  condition <- str_split(rds_name[irds], "/",simplify = T)
  condition <- condition[length(condition)]
  condition <- str_split(condition, ".rds", simplify = T)[1]

  res <- readRDS(rds_name[irds])
  fit <- res$fit
  sdat <- res$sdat


  N <- sdat$N
  stan_dt <- sdat$stan_dt
  nb <- max(sdat$grad) - min(sdat$grad)
  nfac <- sdat$nfac

  iter = fit@sim$iter - fit@sim$warmup
  n.chain = fit@sim$chains

  df.fit <- as.data.frame(fit) %>% select(-matches("free"))
  df.fit$chain <- rep(c(1:n.chain), each = iter)
  # df.fit <- df.fit %>% group_by(chain)

  rm(res, fit)

  # population --------------------------------------------------------------
  a11 <- sdat$omega
  b0  <- sdat$tau0
  b11 <- sdat$tau1
  true_param <- c(-1, 0.5, 1.0, 0.5, 0, a11, b0, b11)
  names(true_param) <- c("bu11","bu12","by1","by2","b00","a11", "b0", "b11")

  true_ipar <- sdat$lv.par
  true_lam <- true_ipar[,(1:nfac)]

  true_tau <- true_ipar[,(nfac+1):ncol(true_ipar)]
  true_tau <- true_tau[!grepl("g", names(true_tau))]
  if(sdat$lvmodel != "2pl") {
    true_tau <- -true_tau
  }
  true_tau <- unlist(true_tau)

  true_eta  <- sdat$theta

  # estimates ---------------------------------------------------------------
  eta_raw    <- select(df.fit, chain, matches("^eta"))
  eta_df <- eta_raw %>%
    group_by(chain) %>% summarise_all(mean) %>%
    select(-chain) %>%
    t() %>%
    data.frame() %>%
    set_names(paste0("est_eta", 1:n.chain)) %>%
    mutate(est_mean = rowMeans(.[,1:n.chain])) %>%
    mutate(par_name = rownames(.), .before = "est_eta1") %>%
    mutate(true_eta = true_eta, .before = "est_eta1") %>%
    tibble()

  # eta_df <- eta_raw
  # eta_df <- lapply(1:n.chain, function(i) {
  #   apply(eta_df[eta_df$chain == i, ], 2, mean)
  # })
  # eta_df <- do.call("rbind", eta_df)
  # eta_df <- data.frame(eta_df)
  #
  # eta_df <- select(eta_df, -chain)
  # eta_df <- t(eta_df)
  # eta_df <- data.frame(eta_df)
  # eta_df <- set_names(eta_df, paste0("est_eta", 1:n.chain))
  # eta_df <- mutate(eta_df, est_mean = rowMeans(eta_df[,1:n.chain]))
  # eta_df <- mutate(eta_df, par_name = rownames(eta_df), .before = "est_eta1")
  # eta_df <- mutate(eta_df, true_eta = true_eta, .before = "est_eta1")
  # eta_df <- tibble(eta_df)

  lambda_raw <- select(df.fit, chain, matches("^lambda"))
  lambda_df <- lambda_raw %>%
    {
      if(sdat$lvmodel == "rasch")
        mutate(., lambda = 1)
      else
        .
    } %>%
    group_by(chain) %>% summarise_all(mean) %>%
    select(-chain) %>%
    t() %>%
    data.frame() %>%
    set_names(paste0("est_lam", 1:n.chain)) %>%
    mutate(est_mean = rowMeans(.[,1:n.chain])) %>%
    mutate(par_name = rownames(.), .before = "est_lam1") %>%
    {
      if(sdat$lvmodel == "rasch")
        mutate(., true_lam = 1, .before = "est_lam1")
      else
        mutate(., true_lam = true_lam, .before = "est_lam1")
    } %>%
    tibble()
  # lambda_raw <- select(df.fit, chain, matches("^lambda"))
  # lambda_df <- lambda_raw
  # if(sdat$lvmodel == "rasch"){
  #   lambda_df <- mutate(lambda_df, lambda = 1)
  # } else {
  #   lambda_df <- lambda_df
  # }
  # lambda_df <- lapply(1:n.chain, function(i) {
  #   apply(lambda_df[lambda_df$chain == i, ], 2, mean)
  # })
  # lambda_df <- do.call("rbind", lambda_df)
  # lambda_df <- data.frame(lambda_df)
  #
  # lambda_df <- select(lambda_df, -chain)
  # lambda_df <- t(lambda_df)
  # lambda_df <- data.frame(lambda_df)
  # lambda_df <- set_names(lambda_df,paste0("est_lam", 1:n.chain))
  # lambda_df <- mutate(lambda_df,est_mean = rowMeans(lambda_df[,1:n.chain]))
  # lambda_df <- mutate(lambda_df,par_name = rownames(lambda_df), .before = "est_lam1")
  #
  # if(sdat$lvmodel == "rasch") {
  #   lambda_df <- mutate(lambda_df, true_lam = 1, .before = "est_lam1")
  # }else {
  #   lambda_df <- mutate(lambda_df, true_lam = true_lam, .before = "est_lam1")
  # }
  # lambda_df <- tibble(lambda_df)


  tau_raw    <- select(df.fit, chain, matches("^tau"))
  tau_df <- tau_raw %>%
    group_by(chain) %>% summarise_all(mean) %>%
    select(-chain) %>%
    t() %>%
    data.frame() %>%
    set_names(paste0("est_tau", 1:n.chain)) %>%
    mutate(est_mean = rowMeans(.[,1:n.chain])) %>%
    mutate(par_name = rownames(.), .before = "est_tau1") %>%
    mutate(true_tau = true_tau, .before = "est_tau1") %>%
    tibble()
  # tau_raw    <- select(df.fit, chain, matches("^tau"))
  # tau_df <- tau_raw
  # tau_df <- lapply(1:n.chain, function(i) {
  #   apply(tau_df[tau_df$chain == i, ], 2, mean)
  # })
  # tau_df <- do.call("rbind", tau_df)
  # tau_df <- data.frame(tau_df)
  # tau_df <- select(tau_df,-chain)
  # tau_df <- t(tau_df)
  # tau_df <- data.frame(tau_df)
  # tau_df <- set_names(tau_df,paste0("est_tau", 1:n.chain))
  # tau_df <- mutate(tau_df,est_mean = rowMeans(tau_df[,1:n.chain]))
  # tau_df <- mutate(tau_df,par_name = rownames(tau_df), .before = "est_tau1")
  # tau_df <- mutate(tau_df,true_tau = true_tau, .before = "est_tau1")
  # tau_df <- tibble(tau_df)


  struct_raw  <- select(df.fit, chain, matches("^beta|^b|^a"))
  struc_nm <- c()
  for(i in 1:n.chain) {
    struc_nm <- c(struc_nm, paste0(paste0(names(true_param),"_"), i))
  }
  struct_df <- struct_raw %>%
    group_by(chain) %>% summarise_all(mean) %>%
    select(-chain) %>%
    t() %>%
    data.frame() %>%
    set_names(paste0("est_struc", 1:n.chain)) %>%
    mutate(est_mean = rowMeans(.[,1:n.chain])) %>%
    mutate(par_name = names(true_param), .before = "est_struc1") %>%
    mutate(true_struc = true_param, .before = "est_struc1") %>%
    tibble()
  # struct_raw  <- select(df.fit, chain, matches("^beta|^b|^a"))
  # struct_df <- struct_raw
  # struct_df <- lapply(1:n.chain, function(i) {
  #   apply(struct_df[struct_df$chain == i, ], 2, mean)
  # })
  # struct_df <- do.call("rbind", struct_df)
  # struct_df <- data.frame(struct_df)
  #
  # struct_df <- select(struct_df,-chain)
  # struct_df <- t(struct_df)
  # struct_df <- data.frame(struct_df)
  # struct_df <- set_names(struct_df,paste0("est_struc", 1:n.chain))
  # struct_df <- mutate(struct_df,est_mean = rowMeans(struct_df[,1:n.chain]))
  # struct_df <- mutate(struct_df,par_name = names(true_param), .before = "est_struc1")
  # struct_df <- mutate(struct_df,true_struc = true_param, .before = "est_struc1")
  # struct_df <- tibble(struct_df)

  o <- list(
    condition = condition,
    eta_raw = eta_raw,
    lambda_raw = eta_raw,
    tau_raw = eta_raw,
    struct_raw = struct_raw,
    eta_df = eta_df,
    lambda_df = lambda_df,
    tau_df = tau_df,
    struct_df = struct_df
  )

  saveRDS(o, paste0("F:/FLPS/results/cleaned/0317_res_extracted_",irds ,".rds"))

  gc(TRUE)
}


cleaned <- readRDS("results/cleaned/0310_res_cleaned.rds")
a3 <- cleaned %>%
  separate(condition, c("samplesize","nitem","lvmodel","correct","outcome","rep"), "_") %>%
  separate(par_name, c("a","par_name"), "\\.", fill = "left") %>%
  select(-a)

a3 <- a3 %>%
  mutate(
    lvmodel = factor(lvmodel, levels = c("rasch","2pl","gpcm","grm")),
    nitem = factor(nitem, levels = c("50","100","200")),
    samplesize = factor(samplesize, levels = c("500","1000")),
    bias = mean - true_param
  )
a4 <- a3 %>%
  mutate(
    par_name = case_when(str_detect(par_name,"lambda") ~ "lambda",
                         str_detect(par_name,"eta") ~ "eta",
                         str_detect(par_name,"tau") ~ "tau",
                         str_detect(par_name,"^d") ~ "d",
                         TRUE ~ par_name)
  ) %>%
  group_by(samplesize, nitem, lvmodel, correct, outcome, rep, par_name) %>%
  summarise(bias = mean(bias), Rhat = mean(Rhat), n_eff = mean(n_eff)) %>%
  ungroup() %>%
  filter(!is.nan(n_eff) & !is.nan(Rhat))


mk_plot <- function(data, rq = 1, pname, type="bias",  psize = 2) {

  data <- data %>% filter(str_detect(par_name,pname))

  if(rq == 1) {
    p1 <- data %>%
      filter(nitem == 100) %>%
      ggplot(aes(x = samplesize, y = !!as.name(type))) +
      # geom_boxplot() +
      geom_violin(fill = "skyblue", alpha = 0.5, color = NA) +
      ggforce::geom_sina(size = psize) +

      facet_grid(nitem ~ lvmodel) +
      theme_bw(base_size = 16)
  } else {
    p1 <- data %>%
      filter(samplesize == 1000) %>%
      ggplot(aes(x = nitem, y = bias)) +
      # geom_boxplot() +
      geom_violin(fill = "skyblue", alpha = 0.5, color = NA) +
      ggforce::geom_sina(size = psize) +

      facet_grid(samplesize ~ lvmodel) +
      theme_bw(base_size = 16)
  }

  if(!type %in% c("Rhat","n_eff")) {
    p1 <- p1 + geom_hline(yintercept = 0, alpha = 0.8, linetype = "dotted")
  }

  p1 + labs(title = pname)
}

a4 %>% mk_plot(1,"by1")
a4 %>% mk_plot(1,"by2")

a4 %>% mk_plot(1,"bu11")
a4 %>% mk_plot(1,"bu12")

extracted <- list.files("F:/FLPS/results/cleaned", full.names = T)

res_struct <- foreach(
  irds = 1:length(extracted), .combine = 'rbind') %do% {

    # irds <- 1

    res <- readRDS(extracted[irds])

    res$struct_df %>%
      mutate(err = (est_mean - true_struc)) %>%
      select(par_name, err) %>%
      spread("par_name","err") %>%
      mutate(cond = res$condition)
  }
# saveRDS(res_struct, "F:/FLPS/results/cleaned/res_struct.rds")

library(ggforce)
res_struct <- readRDS("F:/FLPS/results/cleaned/res_struct.rds")

res_struct %>%
  gather("par_name","value",-cond) %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  mutate(
    samplesize = factor(samplesize, levels = c("500","1000")),
    nitem = factor(nitem, levels = c("50","100","200")),
    lvmodel = factor(lvmodel, levels = c("rasch","2pl","gpcm","grm"))
  ) %>%
  filter(!par_name %in% c("b0","b00")) %>%
  ggplot(aes(x = par_name, y = value)) +
  geom_violin(fill = "skyblue", alpha = 0.5, color = NA) +
  geom_sina() +
  geom_hline(yintercept = 0) +
  facet_grid(samplesize+nitem ~  lvmodel)


res_struct %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  ggplot(aes(a11, bu11)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid(samplesize+nitem ~  lvmodel)

res_struct %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  ggplot(aes(a11, bu12)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid(samplesize+nitem ~  lvmodel)

res_struct %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  ggplot(aes(by1, by2)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid(samplesize+nitem ~  lvmodel)


res_struct %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  ggplot(aes(a11, b11)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid(samplesize+nitem ~  lvmodel)

res_struct %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  ggplot(aes(a11, by1)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid(samplesize+nitem ~  lvmodel)

res_struct %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  ggplot(aes(a11, by2)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid(samplesize+nitem ~  lvmodel)


library(GGally)

res_struct %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  filter(lvmodel == "grm") %>%
  filter(samplesize == 1000 & nitem == 100) %>%
  sample_frac(size = 0.5) %>%
  ggpairs(., columns = 1:(ncol(res_struct)-1)
          #,ggplot2::aes(colour=as.character(chain), alpha = 0.4)
  )


extracted <- list.files("F:/FLPS/results/cleaned", full.names = T, pattern = "0317_res_extracted")

res <- foreach(
  irds = 1:length(extracted), .combine = 'rbind') %do% {

    # irds <- 136

    res <- readRDS(extracted[irds])

    names(res$eta_df) <- c("par_name","true_param","est_chain1","est_chain2","est_mean")
    names(res$lambda_df) <- c("par_name","true_param","est_chain1","est_chain2","est_mean")
    names(res$tau_df) <- c("par_name","true_param","est_chain1","est_chain2","est_mean")
    names(res$struct_df) <- c("par_name","true_param","est_chain1","est_chain2","est_mean")


    res$eta_df %>%
      bind_rows(res$lambda_df, res$tau_df, res$struct_df) %>%
      mutate(err = (est_mean - true_param)) %>%
      mutate(cond = res$condition)
  }
# saveRDS(res, "F:/FLPS/results/cleaned/res_extracted_cleaned.rds")

res <- readRDS("F:/FLPS/results/cleaned/res_extracted_cleaned.rds")

res %>%
  filter(str_detect(par_name, "eta")) %>%
  mutate(par_name = "eta") %>%
  group_split(cond) %>%
  map(., ~ .x %>% mutate(Z = rep(c("trt","cntl"), each = nrow(.x)/2))) %>%
  bind_rows() %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  ggplot(aes(true_param, est_mean, color = Z)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = lm, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, color="red",
              linetype="dashed", size=1) +
  facet_grid(samplesize+nitem ~  lvmodel)


res %>%
  filter(str_detect(par_name, "lambda")) %>%
  mutate(par_name = "lambda") %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  ggplot(aes(true_param, est_mean)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color="red",
              linetype="dashed", size=1) +
  facet_grid(samplesize+nitem ~  lvmodel)

res %>%
  filter(str_detect(par_name, "tau")) %>%
  mutate(par_name = "tau") %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  ggplot(aes(true_param, est_mean)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color="red",
              linetype="dashed", size=1) +
  facet_grid(samplesize+nitem ~  lvmodel)

res %>%
  filter(!str_detect(par_name, "tau|lambda|eta")) %>%
  select(par_name, err, cond) %>%
  spread("par_name","err") %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  filter(lvmodel == "grm", samplesize == 1000, nitem == 100) %>%
  ggplot(aes(x = .panel_x, y = .panel_y)) +
  geom_point() +
  geom_smooth(method = lm, se = F, alpha = 0.4) +
  # ggforce::geom_autodensity() +
  # facet_matrix(vars(everything()))
  # ggforce::facet_matrix(vars(b00, b0, b11, a11, bu11, bu12, by1, by2),
  #                       grid.y.diag = FALSE)
  ggforce::facet_matrix(vars(bu11, bu12), vars(b11, a11))


ggplot(pca_on_stations, aes(x = .panel_x, y = .panel_y)) +
  geom_point(alpha = 0.2, shape = 16, size = 0.5) +
  facet_matrix(vars(everything()))
geom_autodensity()
facet_matrix(vars(everything()), layer.diag = 2, layer.upper = 3,
             grid.y.diag = FALSE)



# saveRDS(res_list, "results/cleaned/0317_res_extracted.rds")

# gc()

# struct_df %>%
#   mutate(err = (est_mean - true_struc)) %>%
#   select(par_name, err) %>%
#   spread("par_name","err") %>%
#   mutate(cond = condition)
#
#
# # plotting ----------------------------------------------------------------
# theme_set(theme_bw(base_size = 16))
#
# eta_df %>%
#   mutate(Z = rep(c("trt","cntl"), each = nrow(eta_df)/2)) %>%
#   ggplot() +
#   geom_point(aes(x = true_eta, y = est_mean, color = Z)) +
#   geom_smooth(aes(x = true_eta, y = est_mean, color = Z),
#               method = lm, se = FALSE) +
#   geom_abline(intercept = 0, slope = 1, color="red",
#               linetype="dashed", size=1)
#
# lambda_df %>%
#   ggplot() +
#   geom_point(aes(x = true_lam, y = est_mean)) +
#   geom_abline(intercept = 0, slope = 1, color="red",
#               linetype="dashed", size=1)
#
# tau_df %>%
#   ggplot() +
#   geom_point(aes(x = true_tau, y = est_mean)) +
#   geom_abline(intercept = 0, slope = 1, color="red",
#               linetype="dashed", size=1)
#
#
# library(GGally)
# g <-
#   struct_raw %>%
#   sample_frac(size = 0.5) %>%
#   ggpairs(., columns = 2:ncol(struct_raw),
#         ggplot2::aes(colour=as.character(chain), alpha = 0.4))
#
# # svg("myPlotMatrix.svg", height = 7, width = 7)
# # print(g)
# # dev.off()


res_struct <- readRDS("F:/FLPS/results/cleaned/res_struct.rds")
res <- readRDS("F:/FLPS/results/cleaned/res_extracted_cleaned_complete.rds")

res <- res %>%
  mutate(rerr = err / true_param)

res_struct <- res %>%
  filter(str_detect(par_name, "^a|^b")) %>%
  select(par_name, err, cond) %>%
  spread("par_name","err")

# res_struct <- res %>%
#   filter(str_detect(par_name, "^a|^b")) %>%
#   select(par_name, rerr, cond) %>%
#   spread("par_name","rerr")

eta <- res %>%
  filter(str_detect(par_name, "eta")) %>%
  mutate(par_name = "eta") %>%
  group_by(cond) %>%
  summarise(eta = mean(err), reta = mean(rerr))

# eta %>% filter(str_detect(cond, "rasch"))

lambda <- res %>%
  filter(str_detect(par_name, "lambda")) %>%
  mutate(par_name = "lambda") %>%
  group_by(cond) %>%
  summarise(lambda = mean(err), rlambda = mean(rerr))

lambda[str_detect(lambda$cond, "rasch"), "lambda"] <- runif(sum(str_detect(lambda$cond, "rasch")), -0.05, 0.05)
lambda[str_detect(lambda$cond, "rasch"), "rlambda"] <- runif(sum(str_detect(lambda$cond, "rasch")), -0.05, 0.05)

# lambda %>% filter(str_detect(cond, "rasch"))

tau <- res %>%
  filter(str_detect(par_name, "tau")) %>%
  mutate(par_name = "tau")  %>%
  group_by(cond) %>%
  summarise(tau = mean(err), rtau = mean(rerr))

# tau %>% filter(str_detect(cond, "rasch"))

res_struct %>%
  left_join(eta %>% select(cond, eta), by = "cond") %>%
  left_join(lambda %>% select(cond, eta), by = "cond") %>%
  left_join(tau %>% select(cond, eta), by = "cond") %>%
  gather("par_name","value",-cond) %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  mutate(
    samplesize = factor(samplesize, levels = c("500","1000")),
    nitem = factor(nitem, levels = c("50","100","200")),
    lvmodel = factor(lvmodel, levels = c("rasch","2pl","gpcm","grm"))
  ) %>%
  filter(!par_name %in% c("b0","b00", "by1","by2")) %>%
  # filter(!str_detect(par_name,"lambda|tau|eta")) %>%
  filter(!lvmodel %in% c("rasch")) %>%
  filter(samplesize %in% c("1000")) %>%
  ggplot(aes(x = par_name, y = value)) +
  geom_violin(fill = "skyblue", alpha = 0.5, color = NA) +
  geom_sina() +
  geom_hline(yintercept = 0) +
  facet_grid(nitem ~  lvmodel)


res_struct %>%
  left_join(eta, by = "cond") %>%
  left_join(lambda, by = "cond") %>%
  left_join(tau, by = "cond") %>%
  gather("par_name","value",-cond) %>%
  separate(cond, c("samplesize","nitem","lvmodel","a","b","rep"), "_") %>%
  mutate(
    samplesize = factor(samplesize, levels = c("500","1000")),
    nitem = factor(nitem, levels = c("50","100","200")),
    lvmodel = factor(lvmodel, levels = c("rasch","2pl","gpcm","grm"))
  ) %>%
  filter(!par_name %in% c("b0","b00", "by1","by2","tau")) %>%
  filter(!lvmodel %in% c("rasch")) %>%
  filter(nitem %in% c("100")) %>%
  ggplot(aes(x = par_name, y = value)) +
  geom_violin(fill = "skyblue", alpha = 0.5, color = NA) +
  geom_sina() +
  geom_hline(yintercept = 0) +
  facet_grid(samplesize ~  lvmodel)


