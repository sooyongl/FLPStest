#' generate true eta (theta)
#'
genTrueEta <- function(N = 1000, R2eta = 0.5, linear = T) {

  eta.res = 1 - R2eta

  x1 = rnorm(N, 0, 1)
  x1sq = x1^2
  x2 = rbinom(N, 1, .5)

  if(linear){
    # linear
    eta <- -x1 + 0.5*x2 + rnorm(N, 0, eta.res)

    data <- cbind(x1, x2, eta)

  } else {
    # non-linear
    eta <- -x1 + 0.5*x1sq + 0.5*x2 + rnorm(N, 0, eta.res)

    data <- cbind(x1, x1sq, x2, eta)
  }

  return(data)
}

genTrueEta.lgm <- function(N = 1000, R2eta = 0.5, linear = T, nfac = 2, gmean) {

  eta.res = 1 - R2eta

  x1 = rnorm(N, 0, 1)
  x1sq = x1^2
  x2 = rbinom(N, 1, .5)

  eta1 <- lvinfo$growth_mean[1] + -x1 + 0.5*x2
  eta2 <- lvinfo$growth_mean[2] + -x1 + 0.5*x2

  resi <- MASS::mvrnorm(
    N, c(0,0),
    Sigma = matrix(c(eta.res, 0, 0, eta.res), ncol = 2),
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

  return(data)
}


#' generate outcome (Y)
#'
genOutcome <- function(data, R2Y, omega, tau0, tau1, linear = T) {

  Y.res = 1 - R2Y
  N <- dim(data)[1]
  Z <- data[,"Z"]
  eta <- data[, grep("eta", colnames(data))]
  x1 <- data[,"x1"]
  x2 <- data[,"x2"]

  if(!is.null(dim(eta))) {
    omega <- rep(omega, dim(eta)[2])
    tau1 <- rep(tau1, dim(eta)[2])
  }

  if(linear) {

    Y <- tau0*Z + matrix(eta)%*%omega + (Z*matrix(eta))%*%tau1 + x1 + 0.5*x2 + rnorm(N, 0, Y.res)

  } else {

    x1sq <- data[,"x1sq"]
    Y <- tau0*Z + matrix(eta)%*%omega + (Z*matrix(eta))%*%tau1 + x1 + 0.5*x1sq + 0.5*x2 + rnorm(N, 0, Y.res)

  }

  return(c(Y))
}
