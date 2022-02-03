#' S3 generic for Outcome data generation
#'
genOutcome <- function(Data, ...) {
  UseMethod("genOutcome", Data)
}

#' generate outcome (Y)
#'
genOutcome.default <- function(Data) {

  N      <- Data$N
  nsec   <- Data$nsec
  R2Y    <- Data$R2Y
  theta  <- Data$theta
  xdata  <- Data$x
  nfac   <- Data$nfac

  R2Y    <- Data$R2Y
  omega  <- Data$omega
  tau0   <- Data$tau0
  tau1   <- Data$tau1
  linear <- Data$linear

  section <- Data$section
  studentM <- Data$studentM
  grad <- Data$grad
  lv.resp <- Data$lv.resp

  a_idx <- gen_a_idx(nsec, nfac)
  fi_idx <- detect_firstitem(a_idx)

  Y.res = 1 - R2Y
  eta <- theta
  x1 <- xdata[,"x1"]
  x2 <- xdata[,"x2"]
  Z <- rep(c(1,0), each=N/2)

  if(!is.null(dim(eta))) {
    omega <- rep(omega, dim(eta)[2])
    tau1 <- rep(tau1, dim(eta)[2])
    n.eta <- ncol(eta)
  } else {
    n.eta <- 1
  }

  if(linear) {
    Y <- tau0*Z + matrix(eta, ncol=n.eta)%*%matrix(omega) + (Z*matrix(eta, ncol=n.eta))%*%tau1 + x1 + 0.5*x2 + rnorm(N, 0, Y.res)
  } else {
    x1sq <- xdata[,"x1sq"]
    Y <- tau0*Z + matrix(eta, ncol=n.eta)%*%omega + (Z*matrix(eta, ncol=n.eta))%*%tau1 + x1 + 0.5*x1sq + 0.5*x2 + rnorm(N, 0, Y.res)
  }

  Data$stan_dt <- list(
    # data info
    nsecWorked = length(section),
    nstud = N,
    nsec = nsec,
    nfac = nfac,
    min_k = min(grad),
    max_k = max(grad),
    ncov = ncol(xdata),
    # index
    studentM = studentM,
    section = section,
    lambda_prior = obv_lambda(obs.v.partial = lv.resp[1:(N/2), ], a_idx),
    factoridx  = a_idx,
    firstitem = fi_idx,
    # data
    grad = grad,
    X = xdata,
    Z = Z,
    Y = c(Y)
  )

  return(Data)
}

#' generate outcome (Y)
#'
genOutcome.lgm <- function(Data) {

  Data <- genOutcome.default(Data)

  Data$stan_dt$gmean = Data$lvinfo$growth_mean
  Data$stan_dt$nfac = Data$lvinfo$nfac
  Data$stan_dt$time_loading = Data$lvinfo$time_loading

  return(Data)

}

#' mixture
#'
genOutcome.mixture <- function(Data) {

  N      <- Data$N
  nsec   <- Data$nsec
  R2Y    <- Data$R2Y
  theta  <- Data$theta
  xdata  <- Data$x

  R2Y    <- Data$R2Y
  omega  <- Data$omega
  tau0   <- Data$tau0
  tau1   <- Data$tau1
  linear <- Data$linear

  section <- Data$section
  studentM <- Data$studentM
  grad <- Data$grad
  lv.resp <- Data$lv.resp

  Y.res = 1 - R2Y
  eta <- theta
  x1 <- xdata[,"x1"]
  x2 <- xdata[,"x2"]

  Y <- rep(0, N)

  idx.c1 <- which(lv.resp[,grep("class",colnames(lv.resp))]==1)
  Y[idx.c1] <- 0 + 0*Z[idx.c1]

  idx.c2 <- which(lv.resp[,grep("class",colnames(lv.resp))]==2)
  Y[idx.c2] <- omega + tau1*Z[idx.c2]

  if(linear) {
    Y <- tau0*Z + 1*x1 + 0.5*x2 + rnorm(N, 0, Y.res)
  } else {
    x1sq <- xdata[,"x1sq"]
    Y <- tau0*Z + 1*x1 + 0.5*x1sq + 0.5*x2 + rnorm(N, 0, Y.res)
  }

  Data$stan_dt <- list(
    nsecWorked = length(section),
    nstud = N,
    nsec = nsec,
    studentM = studentM,
    section = section,
    grad = grad,
    X = xdata,
    ncov = ncol(xdata),
    nclass = lvinfo$nclass,
    Z = Z,
    Y = Y
  )

  return(Data)
}
