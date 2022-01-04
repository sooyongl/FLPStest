#' Generate Fully Latent Principal Stratification data
#'
#' @description
#' \code{\link{makeDat}} is a function for generating a data based on the given
#' information.
#' @param N a numeric indicating sample size.
#' @param R2Y a numeric indicating predictive power of covariates.
#' @param omega a numeric indicating the relationship between eta_T and Y_C.
#' @param tau0 a numeric indicating the treatment eff when eta_T=0.
#' @param tau1 a numeric indicating the relationship between eta_T and Y_T-Y_C;
#' (for now, single level (not multilevel), linear, normal).
#' @param lambda a numeric indicating the mean of Worked problems/person.
#' @param R2eta a numeric indicating Predictive power of latent variable (extent to which covariates predict eta).
#' @param nsec a numeric indicating the number of maximum sections given to students.
#' @param lvmodel a character specifying a type of latent variable model.
#' @return a list containing the data for running FLPS.
#'
#' @examples
#' sdat <- makeDat(
#'   N = 100,
#'   R2Y = 0.2,
#'   omega = 0.2,
#'   tau0 = 0.4,
#'   tau1 = -0.2,
#'   lambda = .6,
#'   R2eta = 0.5,
#'   linear = T,
#'   nsec = 10,
#'   lvmodel = "2PL",
#'   lvinfo = data.frame(a = runif(20, 0.7, 1.4), b = rnorm(20), g = rep(0, 20))
#' )
#'
#' @export
makeDat <- function(N,R2Y,omega,tau0,tau1,lambda,R2eta,linear,nsec,lvmodel,lvinfo){

  lvmodel <- tolower(lvmodel)

  # Generate True eta -------------------------------------------------------
  ## generate eta
  if(lvmodel == "lgm") {
    data <- genTrueEta.lgm(N, R2eta, linear, lvinfo$nfac, lvinfo$growth_mean)
  } else {
    data <- genTrueEta(N, R2eta, linear)
  }

  # Generate LVM data -------------------------------------------------------
  # info <- parsForLVM(theta = data[,grep("eta", colnames(data))], nsec = nsec, data_type = lvmodel, lv_info = lvinfo)
  # info.data <- generateLV(info); # methods(generate)

  # lv.par <- info.data$lv.par
  # grad <- info.data$resp
  #
  # # nworked <- sample(1:nsec,N/2,replace=TRUE,prob=dexp(1:nsec,rate=1/lambda))
  # nworked <- rep(floor(nsec * lambda), N/2)
  #
  # studentM <- do.call("c", lapply(seq(N/2),function(n) rep(n,each=nworked[n])))
  # section <- do.call("c", lapply(seq(N/2),
  #                                function(n) {
  #                                  sort(sample(1:nsec, nworked[n],
  #                                              replace = FALSE))}))
  # ss <- cbind(studentM, section)
  # grad <- sapply(1:dim(ss)[1], function(n) grad[ss[n,1], ss[n,2]] )

  lv.data <-
    generateLV(
      N=N,
      nsec=nsec,
      lambda=lambda,
      theta = data[, grep("eta", colnames(data))],
      lv_model = lvmodel,
      lv_info = lvinfo
    )

  ### simulate Y -----------------------------------------------------------

  ### Treatment or Control
  Z <- rep(c(1,0), each=N/2)
  lv.resp <- lv.data$lv.resp

  if(lvmodel %in% c("lca", "lpa")) {

    Y.res = 1 - R2Y
    eta <- data[, "eta"]
    x1 <- data[,"x1"]
    x2 <- data[,"x2"]

    Y <- rep(0, N)

    idx.c1 <- which(lv.resp[,grep("class",colnames(lv.resp))]==1)
    Y[idx.c1] <- 0 + 0*Z[idx.c1]

    idx.c2 <- which(lv.resp[,grep("class",colnames(lv.resp))]==2)
    Y[idx.c2] <- omega + tau1*Z[idx.c2]

    if(linear) {
      Y <- tau0*Z + 1*x1 + 0.5*x2 + rnorm(N, 0, Y.res)
    } else {
      x1sq <- data[,"x1sq"]
      Y <- tau0*Z + 1*x1 + 0.5*x1sq + 0.5*x2 + rnorm(N, 0, Y.res)
    }

    datalist <- list(
      nsecWorked = length(lv.data$section),
      nstud = N,
      nsec = nsec,
      studentM = lv.data$studentM,
      section = lv.data$section,
      grad = lv.data$grad,
      X = data[, grepl("x", colnames(data))],
      ncov = sum(grepl("x", colnames(data))),
      nclass = lvinfo$nclass,
      Z = Z,
      Y = Y,
      lv.par = lv.data$lv.par,
      true_eta = data[,"eta"],
      lv.rep = lv.data$lv.resp
    )

  } else if(lvmodel %in% c("lgm")) {
    Y <- genOutcome(cbind(Z,data), R2Y, omega, tau0, tau1, linear)

    datalist <- list(
      nsecWorked = length(lv.data$section),
      nstud = N,
      nsec = nsec,
      studentM = lv.data$studentM,
      section = lv.data$section,
      grad = lv.data$grad,
      X = data[, grepl("x", colnames(data))],
      ncov = sum(grepl("x", colnames(data))),
      gmean = lvinfo$growth_mean,
      nfac = lvinfo$nfac,
      time_loading = lvinfo$time_loading,
      Z = Z,
      Y = Y,
      true_eta = data[,grepl("eta", colnames(data))],
      lv.par = lv.data$lv.par,
      lv.rep = lv.data$lv.resp
    )


  } else {

    Y <- genOutcome(cbind(Z,data), R2Y, omega, tau0, tau1, linear)

    datalist <- list(
      nsecWorked = length(lv.data$section),
      nstud = N,
      nsec = nsec,
      max_k = max(lv.data$grad),
      lambda_prior = obv_lambda(lv.resp[1:(N/2), ]),
      studentM = lv.data$studentM,
      section = lv.data$section,
      grad = lv.data$grad,
      X = data[, grepl("x", colnames(data))],
      ncov = sum(grepl("x", colnames(data))),
      Z = Z,
      Y = Y,
      true_eta = data[,grepl("eta", colnames(data))],
      lv.par = lv.data$lv.par,
      lv.rep = lv.data$lv.resp
    )
  }

  return(datalist)
}

#' Generate a matrix style data for simulation
#'
#' @description
#' \code{\link{makeInpDat}} is a function for generating a data based on the given
#' information.
#' @param N a numeric indicating sample size.
#' @param R2Y a numeric indicating predictive power of covariates.
#' @param omega a numeric indicating the relationship between eta_T and Y_C.
#' @param tau0 a numeric indicating the treatment eff when eta_T=0.
#' @param tau1 a numeric indicating the relationship between eta_T and Y_T-Y_C;
#' (for now, single level (not multilevel), linear, normal).
#' @param lambda a numeric indicating the mean of Worked problems/person.
#' @param R2eta a numeric indicating Predictive power of latent variable
#' (extent to which covariates predict eta).
#' @param nsec a numeric indicating the number of maximum sections given to students.
#' @param lvmodel a character specifying a type of latent variable model.
#' @return a list containing latent variable model information, true eta,
#' and a matrix containing the data for running FLPS.
#'
#' @examples
#' sdat <- makeInpData(
#'   N = 100,
#'   R2Y = 0.2,
#'   omega = 0.2,
#'   tau0 = 0.13,
#'   tau1 = -0.06,
#'   lambda = 10,
#'   R2eta = 0.5,
#'   nsec = 10,
#'   linear = T,
#'   lvmodel = "sem",
#'   lvinfo  = 1
#' )
#' @export
makeInpData <- function(N,R2Y,omega,tau0,tau1,lambda,R2eta,linear,nsec,lvmodel, lvinfo){

  lvmodel <- tolower(lvmodel)

  ## generate covariates ----------------------------------------------------

  ### Treatment or Control
  Z <- rep(c(1,0), each=N/2)

  # Generate True eta -------------------------------------------------------
  ## generate eta
  data <- genTrueEta(N, R2eta, linear)

  # Generate LVM data -------------------------------------------------------
  info <- parsForLVM(theta = data[,"eta"], nsec = nsec, data_type = lvmodel, lv_info = lvinfo)
  info.data <- generate(info); # methods(generate)

  lv.par <- info.data$lv.par
  grad <- info.data$resp

  # nworked <- sample(1:nsec,N/2,replace=TRUE,prob=dexp(1:nsec,rate=1/lambda))
  nworked <- rep(floor(nsec * lambda), N/2)

  studentM <- do.call("c", lapply(seq(N/2),function(n) rep(n,each=nworked[n])))

  section <- do.call("c", lapply(seq(N/2),
                                 function(n) {
                                   sort(sample(1:nsec, nworked[n],
                                               replace = FALSE))}))
  ss <- cbind(studentM, section)

  d1 <- matrix(rep(NA, nsec*(N/2)), ncol = nsec)
  s1 <- split(ss[,2], studentM);
  for(i in 1:length(s1)) {d1[i, s1[[i]]] <- grad[i, s1[[i]]]}

  d2 <- matrix(rep(NA, nsec*(N/2)), ncol = nsec)
  d3 <- rbind(d1, d2)

  ### simulate Y -------------------------------------------------------------
  if(lvmodel %in% c("lca", "lpa")) {
    Y.res = 1 - R2Y
    eta <- data[, "eta"]
    x1 <- data[,"x1"]
    x2 <- data[,"x2"]

    Y <- rep(0, N)

    idx.c1 <- which(info.data$resp[, "class"]==0)
    Y[idx.c1] <- 0 + 0*Z[idx.c1]

    idx.c2 <- which(info.data$resp[, "class"]==1)
    Y[idx.c2] <- omega + tau1*Z[idx.c2]

    if(linear) {
      Y <- tau0*Z + 1*x1 + 0.5*x2 + rnorm(N, 0, Y.res)
    } else {
      x1sq <- data[,"x1sq"]
      Y <- tau0*Z + 1*x1 + 0.5*x1sq + 0.5*x2 + rnorm(N, 0, Y.res)
    }

  } else {

    Y <- genOutcome(cbind(Z,data), R2Y, omega, tau0, tau1, linear)
  }

  inp_data <- data.frame(Y, Z, data[, grepl("x", colnames(data))], d3)

  list(
    lv.rep = info.data$resp,
    lv.par = lv.par,
    true_eta = data[,"eta"],
    inp_data = inp_data
  )
}
