plotting <- function(data){
  theme_set(theme_bw(base_size = 36))
  data %>%
    ggplot(aes(x = par_name, y = err)) +
    geom_violin(
      trim=F,
      fill = "skyblue", alpha = 0.5, color = NA) +
    ggforce::geom_sina(size = 3) +
    geom_hline(yintercept = 0) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "black",
      size = 3,
      shape = 24,
      alpha = 0.8,
      fill = "red"
    )
}

extract_pars <- function(fit, sdat, two) {
  N <- sdat$N
  stan_dt <- sdat$stan_dt
  nb <- max(sdat$grad) - min(sdat$grad)
  nfac <- sdat$nfac

  iter = fit@sim$iter - fit@sim$warmup
  n.chain = fit@sim$chains

  df.fit <- as.data.frame(fit) %>% select(-matches("free"))
  df.fit$chain <- rep(c(1:n.chain), each = iter)
  # df.fit <- df.fit %>% group_by(chain)

  # population --------------------------------------------------------------
  a11 <- sdat$omega
  b0  <- sdat$tau0
  b11 <- sdat$tau1
  # true_param <- c(-1, 0.5, 1.0, 0.5, 0, a11, b0, b11)
  # names(true_param) <- c("bu11","bu12","by1","by2","b00","a11", "b0", "b11")

  # true_param <- c(-1, 0.5)
  # names(true_param) <- c("bu11","bu12")


  if(two) {
    true_param <- c(-1, 0.5)
    names(true_param) <- c("bu11","bu12")
  } else {
    true_param <- c(-1, 0.5, 1.0, -0.5)
    names(true_param) <- c("bu11","bu12","bu13","bu14")
  }
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

    mutate(est_mean = est_eta1) %>%

    mutate(par_name = rownames(.), .before = "est_eta1") %>%

    # mutate(est_mean = rowMeans(.[,1:n.chain])) %>%
    # mutate(par_name = rownames(.), .before = "est_eta1") %>%

    mutate(true_eta = true_eta, .before = "est_eta1") %>%
    tibble()

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

    mutate(est_mean = est_lam1) %>%
    # mutate(est_mean = rowMeans(.[,1:n.chain])) %>%

    mutate(par_name = rownames(.), .before = "est_lam1") %>%
    {
      if(sdat$lvmodel == "rasch")
        mutate(., true_lam = 1, .before = "est_lam1")
      else
        mutate(., true_lam = true_lam, .before = "est_lam1")
    } %>%
    tibble()

  tau_raw    <- select(df.fit, chain, matches("^tau"))
  tau_df <- tau_raw %>%
    group_by(chain) %>% summarise_all(mean) %>%
    select(-chain) %>%
    t() %>%
    data.frame() %>%
    set_names(paste0("est_tau", 1:n.chain)) %>%
    mutate(est_mean = est_tau1) %>%
    # mutate(est_mean = rowMeans(.[,1:n.chain])) %>%
    mutate(par_name = rownames(.), .before = "est_tau1") %>%
    mutate(true_tau = true_tau, .before = "est_tau1") %>%
    tibble()

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
    mutate(est_mean = est_struc1) %>%
    #mutate(est_mean = rowMeans(.[,1:n.chain])) %>%
    mutate(par_name = names(true_param), .before = "est_struc1") %>%
    mutate(true_struc = true_param, .before = "est_struc1") %>%
    tibble()

  o <- list(
    # condition = condition,
    # eta_raw = eta_raw,
    # lambda_raw = eta_raw,
    # tau_raw = eta_raw,
    # struct_raw = struct_raw,
    eta_df = eta_df,
    lambda_df = lambda_df,
    tau_df = tau_df,
    struct_df = struct_df
  )
  o
}
