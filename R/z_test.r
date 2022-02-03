clean_temp <- function(fit, sdat) {
  N <- sdat$N
  stan_dt <- sdat$stan_dt
  nb <- max(sdat$grad) - min(sdat$grad)
  nfac <- sdat$nfac

  iter = fit@sim$iter - fit@sim$warmup
  chain = fit@sim$chains

  # item param
  df.fit <- as.data.frame(fit)
  df.fit$chain <- rep(c(1:chain), each = iter)
  df.fit <- df.fit %>% group_by(chain)


  lambda <- df.fit %>%
    select(matches("^lambda\\[")) %>%
    summarise_all(mean) %>%
    select(-chain) %>%
    matrix(., ncol = nfac)

  tau <- df.fit %>%
    select(chain, matches("^tau\\[")) %>%
    summarise_all(mean) %>%
    select(-chain) %>%
    matrix(., ncol = nb)

  comb_lambda <- cbind(sdat$lv.par[,1:nfac],lambda)
  comb_lambda <- apply(comb_lambda, 2, as.numeric)
  comb_tau    <- cbind(sdat$lv.par[,(nfac+1):ncol(sdat$lv.par)], tau)
  comb_tau    <- apply(comb_tau, 2, as.numeric)

  eta <- df.fit %>%
    select(chain, matches("^eta")) %>%
    summarise_all(mean) %>%
    select(-chain) %>%
    matrix(., ncol = nfac, nrow = N)

  comb_eta <- cbind(sdat$theta, eta)
  comb_eta <- apply(comb_eta, 2, as.numeric)

  # apply(comb_eta, 2, function(x) mean(unlist(x)))
  # apply(comb_eta, 2, function(x) var(unlist(x)))
  #
  #
  # plot(unlist(comb_eta[,1]), unlist(comb_eta[,3]))
  # plot(unlist(comb_eta[,2]), unlist(comb_eta[,4]))

  true_param <- check_flps(stan_dt)
  # colnames(stan_dt$X) <- paste0("X", 1:ncol(stan_dt$X))
  # # main effect ------------------------------------------
  # true_data <- data.frame(Y = stan_dt$Y, stan_dt$X, eta = sdat$theta, Z = stan_dt$Z)
  #
  # etas <- names(true_data)[str_detect(names(true_data), "eta")]
  # Xs <- names(true_data)[str_detect(names(true_data), "X")]
  #
  # etapart<- paste(etas, collapse = "+")
  # etazpart <- paste(paste0(etas, "*Z"), collapse = "+")
  # true_form <- as.formula(glue::glue("Y ~ Z + {etapart} + {etazpart} + X1 + X2"))
  # true_param <- lm(true_form, data = true_data)
  # true_param <- coefficients(true_param)
  #
  # true_param1 <- lm(eta.eta1 ~ X1 + X2, data = true_data)
  # true_param2 <- lm(eta.eta2 ~ X1 + X2, data = true_data)
  # true_param12 <- c(coefficients(true_param1)[2:3],
  #                   coefficients(true_param2)[2:3])
  #
  # true_param <- c(true_param, true_param12)
  #
  # names(true_param) <- c("b00", "b0", "a11","a12", "by1","by2",
  #                        "b11","b12", "bu11","bu21","bu12","bu22")
  # true_param <- true_param[c("b00", "b0", "b11","b12", "a11","a12",
  #                            "by1","by2", "bu11","bu21","bu12","bu22")]
  bu <- names(true_param)[grep("bu", names(true_param))]
  b1 <- names(true_param)[grep("b1", names(true_param))]
  a1 <- names(true_param)[grep("a1", names(true_param))]
  by <- names(true_param)[grep("by", names(true_param))]

  est_param <- df.fit %>%
    select(matches("^b00|^b0|b1|a1|betaY|betaU")) %>%
    summarise_all(mean) %>%
    select(-chain) %>%
    set_names(c(bu, by, "b00", a1, "b0", b1)) %>%
    select(names(true_param))

  flps_param <- bind_rows(true = true_param, est = est_param, .id = "type")

  list(flps_param=flps_param,
       comb_lambda=comb_lambda,
       comb_tau=comb_tau,
       comb_eta=comb_eta)

}


#' gelman diagnostic with chol.default error addressed.
#'
# my.gelman.diag <- function(x,confidence = 0.95,transform = FALSE,autoburnin = FALSE,multivariate = TRUE) {
#   x <- as.mcmc.list(x)
#   if (nchain(x) < 2)
#     stop("You need at least two chains")
#   if (autoburnin && start(x) < end(x)/2)
#     x <- window(x, start = end(x)/2 + 1)
#   Niter <- niter(x)
#   Nchain <- nchain(x)
#   Nvar <- nvar(x)
#   xnames <- varnames(x)
#   if (transform)
#     x <- gelman.transform(x)
#   x <- lapply(x, as.matrix)
#   S2 <- array(sapply(x, var, simplify = TRUE),
#               dim = c(Nvar, Nvar, Nchain)
#   )
#   W <- apply(S2, c(1, 2), mean)
#   xbar <- matrix(sapply(x, apply, 2, mean, simplify = TRUE),
#                  nrow = Nvar, ncol = Nchain)
#   B <- Niter * var(t(xbar))
#   if (Nvar > 1 && multivariate) {  #ph-edits
#     # CW <- chol(W)
#     #    #This is W^-1*B.
#     # emax <- eigen(
#     #  backsolve(CW, t(backsolve(CW, B, transpose = TRUE)), transpose = TRUE),
#     # symmetric = TRUE, only.values = TRUE)$values[1]
#     emax <- 1
#     mpsrf <- sqrt((1 - 1/Niter) + (1 + 1/Nvar) * emax/Niter)
#   }  else {
#     mpsrf <- NULL
#   }
#
#   w <- diag(W)
#   b <- diag(B)
#   s2 <- matrix(apply(S2, 3, diag), nrow = Nvar, ncol = Nchain)
#   muhat <- apply(xbar, 1, mean)
#   var.w <- apply(s2, 1, var)/Nchain
#   var.b <- (2 * b^2)/(Nchain - 1)
#   cov.wb <- (Niter/Nchain) * diag(var(t(s2), t(xbar^2)) - 2 *
#                                     muhat * var(t(s2), t(xbar)))
#   V <- (Niter - 1) * w/Niter + (1 + 1/Nchain) * b/Niter
#   var.V <- ((Niter - 1)^2 * var.w + (1 + 1/Nchain)^2 * var.b +
#               2 * (Niter - 1) * (1 + 1/Nchain) * cov.wb)/Niter^2
#   df.V <- (2 * V^2)/var.V
#   df.adj <- (df.V + 3)/(df.V + 1)
#   B.df <- Nchain - 1
#   W.df <- (2 * w^2)/var.w
#   R2.fixed <- (Niter - 1)/Niter
#   R2.random <- (1 + 1/Nchain) * (1/Niter) * (b/w)
#   R2.estimate <- R2.fixed + R2.random
#   R2.upper <- R2.fixed + qf((1 + confidence)/2, B.df, W.df) *
#     R2.random
#   psrf <- cbind(sqrt(df.adj * R2.estimate), sqrt(df.adj * R2.upper))
#   dimnames(psrf) <- list(xnames, c("Point est.", "Upper C.I."))
#   out <- list(psrf = psrf, mpsrf = mpsrf, B = B, W = W) #added ph
#   class(out) <- "gelman.diag"
#   return( out )
# }
