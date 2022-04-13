#' S3 generic for Outcome data generation
#'
genOutcome <- function(Data, ...) { # Data = sim_info
  UseMethod("genOutcome", Data)
}

#' generate outcome (Y)
#'
genOutcome.default <- function(Data) {

  N      <- Data$N
  nsec   <- Data$nsec
  theta  <- Data$theta
  xdata  <- Data$x
  nfac   <- Data$nfac

  R2Y    <- Data$R2Y
  linear <- Data$linear
  ydist  <- Data$ydist

  omega  <- Data$omega # round(runif(nfac, 0.1, 0.3),3)
  tau0   <- Data$tau0  # round(runif(1, 0.2, 0.4),3)
  tau1   <- Data$tau1  # round(runif(nfac, -0.2, -0.1),3)

  section  <- Data$section
  studentM <- Data$studentM
  grad     <- Data$grad
  lv.resp  <- Data$lv.resp

  a_idx <- gen_a_idx(nsec, nfac)
  fi_idx <- detect_firstitem(a_idx)

  Y.R2  <- R2Y
  eta   <- theta
  x1    <- xdata[,"x1"]
  x2    <- xdata[,"x2"]
  Z     <- rep(c(1,0), each=N/2)

  n.eta <- ifelse(!is.null(dim(eta)),  ncol(eta), 1)

  Y <-
    tau0*Z +
    matrix(eta, ncol=n.eta)%*%matrix(omega) +
    (Z*matrix(eta, ncol=n.eta))%*%tau1 +
    1*x1 +
    0.5*x2

  if(!linear) {
    Y <- Y + 0.5*x1sq
  }

  unex_var <- Y.R2/(1 - Y.R2)*var(Y)
  Y <- Y + rnorm(N, 0, sqrt(unex_var))

  if(ydist == "t") {
    trunc_point <- quantile(Y, c(.1, .9))

    Y[Y < trunc_point[1]] <- trunc_point[1]
    Y[Y > trunc_point[2]] <- trunc_point[2]

  } else if(ydist == "t3") {

    Y <- Y + rq(N, 3)
  }

  Data$stan_dt <- list(
    # data info
    nsecWorked = length(section),
    nstud      = N,
    nsec       = nsec,
    nfac       = nfac,
    min_k      = min(grad),
    max_k      = max(grad),
    ncov       = ncol(xdata),
    # index
    studentM     = studentM,
    section      = section,
    lambda_prior = obv_lambda(obs.v.partial = lv.resp[1:(N/2), ], a_idx),
    factoridx    = a_idx,
    firstitem    = fi_idx,
    # data
    grad   = grad,
    X      = xdata,
    Z      = Z,
    Y      = c(Y)
  )

  return(Data)
}

#' generate outcome (Y)
#'
test_genOutcome <- function(Data) {

  N      <- Data$N
  nsec   <- Data$nsec
  theta  <- Data$theta
  xdata  <- Data$x
  nfac   <- Data$nfac

  R2Y    <- Data$R2Y
  linear <- Data$linear
  ydist  <- Data$ydist

  omega  <- Data$omega # round(runif(nfac, 0.1, 0.3),3)
  tau0   <- Data$tau0  # round(runif(1, 0.2, 0.4),3)
  tau1   <- Data$tau1  # round(runif(nfac, -0.2, -0.1),3)

  section  <- Data$section
  studentM <- Data$studentM
  grad     <- Data$grad
  lv.resp  <- Data$lv.resp

  a_idx <- gen_a_idx(nsec, nfac)
  fi_idx <- detect_firstitem(a_idx)

  Y.R2  <- R2Y
  eta   <- theta
  x1    <- xdata[,"x1"]
  x2    <- xdata[,"x2"]
  x3    <- xdata[,"x3"]
  x4    <- xdata[,"x4"]
  Z     <- rep(c(1,0), each=N/2)

  n.eta <- ifelse(!is.null(dim(eta)),  ncol(eta), 1)

  Y <-
    tau0*Z +
    matrix(eta, ncol=n.eta)%*%matrix(omega) +
    (Z*matrix(eta, ncol=n.eta))%*%tau1 +
    1*x1 +
    0.5*x2 +
    -1*x3 +
    -0.5*x4

  if(!linear) {
    Y <- Y + 0.5*x1sq
  }

  unex_var <- Y.R2/(1 - Y.R2)*var(Y)
  Y <- Y + rnorm(N, 0, sqrt(unex_var))

  if(ydist == "t") {
    trunc_point <- quantile(Y, c(.1, .9))

    Y[Y < trunc_point[1]] <- trunc_point[1]
    Y[Y > trunc_point[2]] <- trunc_point[2]

  } else if(ydist == "t3") {

    Y <- Y + rq(N, 3)
  }

  Data$stan_dt <- list(
    # data info
    nsecWorked = length(section),
    nstud      = N,
    nsec       = nsec,
    nfac       = nfac,
    min_k      = min(grad),
    max_k      = max(grad),
    ncov       = ncol(xdata),
    # index
    studentM     = studentM,
    section      = section,
    lambda_prior = obv_lambda(obs.v.partial = lv.resp[1:(N/2), ], a_idx),
    factoridx    = a_idx,
    firstitem    = fi_idx,
    # data
    grad   = grad,
    X      = xdata,
    Z      = Z,
    Y      = c(Y)
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
