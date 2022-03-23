#' S3 generic for individual latent scores data generation
#'
genTrueEta <- function(Data, ...) { # Data = sim_info
  UseMethod("genTrueEta", Data)
}

#' generate true eta (theta)
#'
genTrueEta.default <- function(Data) {

  N      <- Data$N
  R2eta  <- Data$R2eta
  linear <- Data$linear
  lvinfo <- Data$lvinfo
  nfac   <- Data$nfac

  eta.R2 = R2eta

  x1   <- rnorm(N, 0, 1)
  x1sq <- x1^2
  x2   <- rbinom(N, 1, .5) # rnorm(N, 0, 1) # 

  if(linear){

    X    <- cbind(x1, x2)
    beta <- rbind(rep(-1, nfac), rep(0.5, nfac))

    ETA <- X %*% beta

    exp_var  <- diag(cov(ETA))
    unex_var <- eta.R2/(1 - eta.R2)*exp_var

    RESI <- MASS::mvrnorm(
      N,
      rep(0, nfac),
      # Sigma = diag(eta.res,nfac),
      Sigma = diag(unex_var, nfac),
      empirical = T)

    ETA <- ETA + RESI
    colnames(ETA) <- paste0("eta",1:nfac)

    data <- cbind(X, ETA)
  } else {

    X    <- cbind(x1, x1sq, x2)
    beta <- rbind(rep(-1, nfac), rep(0.5, nfac), rep(0.5, nfac))

    ETA <- X %*% beta

    exp_var  <- diag(cov(ETA))
    unex_var <- eta.R2/(1 - eta.R2)*exp_var

    RESI <- MASS::mvrnorm(
      N,
      rep(0, nfac),
      # Sigma = diag(eta.res,nfac),
      Sigma = diag(unex_var,nfac),
      empirical = T)

    ETA <- ETA + RESI
    colnames(ETA) <- paste0("eta",1:nfac)

    data <- cbind(X, ETA)
  }

  Data$x <- data[, grep("x", colnames(data))]
  Data$theta <- data[, grep("eta", colnames(data))]

  return(Data)
}

#' generate true eta (theta)
#'
genTrueEta.lgm <- function(Data) {

  N <- Data$N
  R2eta <- Data$R2eta
  linear <- Data$linear
  lvinfo <- Data$lvinfo
  nfac  <- lvinfo$nfac
  gmean <- lvinfo$growth_mean

  eta.res = 1 - R2eta

  x1 = rnorm(N, 0, 1)
  x1sq = x1^2
  x2 = rnorm(N, 0, 1) # rbinom(N, 1, .5)

  eta1 <- lvinfo$growth_mean[1] + -x1 + 0.5*x2
  eta2 <- lvinfo$growth_mean[2] + -x1 + 0.5*x2

  resi <- MASS::mvrnorm(
    N, c(0,0),
    Sigma = matrix(c(eta.res, 0.1, 0.1, eta.res), ncol = 2),
    empirical = T)

  eta <- cbind(eta1, eta2) + resi

  data <- cbind(x1, x2, eta)

  # library(lavaan)
  # lavaan::sem(model = "eta1 ~ x1 + x2
  #               eta2 ~ x1 + x2
  #               x1 ~~ x2
  #               eta1 ~~ eta2
  #               x1 ~ 1
  #               x2 ~ 1
  #               eta1 ~ 1
  #               eta2 ~ 1
  #               ", data = data.frame(data),
  #             meanstructure = T) %>% summary()

  Data$x <- data[, grep("x", colnames(data))]
  Data$theta = data[, grep("eta", colnames(data))]

  return(Data)
}
