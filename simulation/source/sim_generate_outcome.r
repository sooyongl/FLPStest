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

  Data$omega0 <- Data$omega
  Data$tau00 <- Data$tau0
  Data$tau10 <- Data$tau1

  omega  <- round(runif(nfac, 0.1, 0.3),3)
  tau0   <- round(runif(1, 0.2, 0.4),3)
  tau1   <- round(runif(nfac, -0.2, -0.1),3)

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

  sdy = sqrt(2)

  omega = omega* (sdy / sd(eta))
  tau0 = tau0* (sdy / sd(Z))
  tau1 = tau1* (sdy / sd(Z*eta))

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

  # unex_var <- Y.R2/(1 - Y.R2)*var(Y)
  unex_var <- Y.R2/(1 - Y.R2)*2
  Y <- Y + rnorm(N, 0, sqrt(unex_var))

  if(ydist == "t") {
    trunc_point <- quantile(Y, c(.1, .9))

    Y[Y < trunc_point[1]] <- trunc_point[1]
    Y[Y > trunc_point[2]] <- trunc_point[2]

  } else if(ydist == "t3") {

    Y <- Y + rq(N, 3)
  }

  Y <- Y - mean(Y)

  Data$omega <- omega
  Data$tau0 <- tau0
  Data$tau1 <- tau1

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

