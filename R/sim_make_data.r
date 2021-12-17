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
#'   tau0 = 0.13,
#'   tau1 = -0.06,
#'   lambda = 10,
#'   R2eta = 0.5,
#'   nsec = 10,
#'   lvmodel = "sem"
#' )
#'
#' @export
makeDat <- function(N,R2Y,omega,tau0,tau1,lambda,R2eta,nsec,lvmodel){

  lvmodel <- tolower(lvmodel)

  ## generate covariates ----------------------------------------------------

  ### Auxiliary Covariates
  x1 <- rnorm(N);
  x2 <- rnorm(N);
  ### Treatment or Control
  Z <- rep(c(1,0),each=N/2)

  # Generate True eta -------------------------------------------------------
  ## generate etas
  random.e <- rnorm(N,0,sqrt(1-R2eta))
  eta <- sqrt(R2eta/2)*(x1-x2) + random.e

  # Generate LVM data -------------------------------------------------------
  info <- parsForLVM(theta = eta, nsec = nsec, data_type = lvmodel)
  info.data <- generate(info); # methods(generate)

  lv.par <- info.data$lv.par
  grad <- info.data$resp

  ## in the application we said the # obs/ student would be pois(lambda)
  ## it looks like a (descretized) version of the exponential fits the CTA1
  ## data much better (tho still not great)

  # nworked <- sample(1:nsec,N/2,replace=TRUE,prob=dexp(1:nsec,rate=1/lambda))
  nworked <- rep(floor(nsec * lambda), N/2)

  ### this results in a somewhat lower mean than lambda

  studentM <- do.call("c", lapply(seq(N/2),function(n) rep(n,each=nworked[n])))

  ## who works which section? Not sure how to do this right
  ## for now, just make it random. this is _not_ true for CT data
  section <- do.call("c", lapply(seq(N/2),
                                 function(n) {
                                   sort(sample(1:nsec, nworked[n],
                                               replace = FALSE))}))
  ss <- cbind(studentM, section)

  grad <- sapply(1:dim(ss)[1], function(n) grad[ss[n,1], ss[n,2]] )

  if(lvmodel == "lca") {

    studItem <- unname(table(studentM))
    cumStudItem <- c(0, cumsum(studItem)[-length(studItem)])

    random.Y <- rnorm(N,0,sqrt(1-R2Y))

    Y <- rep(0, N)

    idx.c1 <- which(info.data$resp$class==0)
    Y[idx.c1] <- 0 + 0*Z[idx.c1]

    idx.c2 <- which(info.data$resp$class==1)
    Y[idx.c2] <- omega + tau1*Z[idx.c2]

    Y <- Y + tau0*Z + sqrt(R2Y/2)*(x2-x1) + random.Y

    varlist <- list(
      nsecWorked = length(section),
      nstud = N,
      nsec = nsec,
      max_k = max(grad),
      lv.par = lv.par,
      studentM = studentM,
      section = section,
      grad = grad,
      X = cbind(x1,x2),
      ncov = 2,
      true_eta = eta,
      Z = Z,
      Y = Y,

      studItem = studItem,
      cumStudItem = cumStudItem,
      lv.rep = info.data$resp
    )

  } else {

    ### simulate Y -------------------------------------------------------------
    random.Y <- rnorm(N,0,sqrt(1-R2Y))

    Y <- tau0*Z + omega*eta + tau1*eta*Z + sqrt(R2Y/2)*(x2-x1) + random.Y

    varlist <- list(
      nsecWorked = length(section),
      nstud = N,
      nsec = nsec,
      max_k = max(grad),
      lv.par = lv.par,
      studentM = studentM,
      section = section,
      grad = grad,
      X = cbind(x1,x2),
      ncov = 2,
      true_eta = eta,
      Z = Z,
      Y = Y
    )
  }

  return(varlist)
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
#' sdat <- makeInpDat(
#'   N = 100,
#'   R2Y = 0.2,
#'   omega = 0.2,
#'   tau0 = 0.13,
#'   tau1 = -0.06,
#'   lambda = 10,
#'   R2eta = 0.5,
#'   nsec = 10,
#'   lvmodel = "sem"
#' )
#' @export
makeInpData <- function(N,R2Y,omega,tau0,tau1,lambda,R2eta,nsec,lvmodel){

  lvmodel <- tolower(lvmodel)

  ## generate covariates ----------------------------------------------------

  ### Auxiliary Covariates
  x1 <- rnorm(N);
  x2 <- rnorm(N);
  ### Treatment or Control
  Z <- rep(c(1,0),each=N/2)

  # Generate True eta -------------------------------------------------------
  ## generate etas
  random.e <- rnorm(N,0,sqrt(1-R2eta))
  eta <- sqrt(R2eta/2)*(x1-x2) + random.e

  # Generate LVM data -------------------------------------------------------
  info <- parsForLVM(theta = eta, nsec = nsec, data_type = lvmodel)
  info.data <- generate(info); # methods(generate)

  lv.par <- info.data$lv.par
  grad <- info.data$resp

  # nworked <- sample(1:nsec,N/2,replace=TRUE,prob=dexp(1:nsec,rate=1/lambda))
  nworked <- rep(floor(nsec * lambda), N/2)


  studentM <- do.call("c", lapply(seq(N/2),function(n) rep(n,each=nworked[n])))

  section <- do.call("c", lapply(seq(N / 2),
                                 function(n) {
                                   sort(sample(1:nsec, nworked[n],
                                               replace = FALSE))}))
  ss <- cbind(studentM, section)

  d1 <- matrix(rep(NA, nsec*(N/2)), ncol = nsec)
  s1 <- split(ss[,2], studentM);
  for(i in 1:length(s1)) { d1[i, s1[[i]]] <- grad[i, s1[[i]]]  }

  d2 <- matrix(rep(NA, nsec*(N/2)), ncol = nsec)
  d3 <- rbind(d1, d2)

  ### simulate Y -------------------------------------------------------------
  random.Y <- rnorm(N,0,sqrt(1-R2Y))
  Y <- tau0*Z + omega*eta + tau1*eta*Z + sqrt(R2Y/2)*(x2-x1) + random.Y

  inp_data <- data.frame(Y, Z, cbind(x1, x2), d3)

  list(
    lv.rep = info.data$resp,
    lv.par = lv.par,
    true_eta = eta,
    inp_data = inp_data
  )
}
