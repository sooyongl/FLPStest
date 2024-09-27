#' Generate Fully Latent Principal Stratification data
#'
#' @description
#' \code{\link{makeDat}} is a function for generating a data based on the given
#' information.
#'
#' @param N a numeric indicating sample size.
#' @param R2Y a numeric indicating predictive power of covariates.
#' @param R2eta a numeric indicating Predictive power of latent variable
#' @param linear a logical
#' @param ydist a character
#' @param lambda a numeric indicating the mean of Worked problems/person.
#'  (extent to which covariates predict eta).
#' @param nsec a numeric indicating the number of maximum sections given to
#'  students.
#' @param nfac a numeric indicating the number of latent factors
#' @param lvmodel a character specifying a type of latent variable model.
#'
#' @return a list containing all the data related to population values and running FLPS.
#'
#' @examples
#' sdat <- makeDat(
#'   N = 100,
#'   R2Y = 0.2,
#'   R2eta = 0.5,
#'   linear = T,
#'   ydist = "n",
#'   lambda = .6,
#'   nsec = 10,
#'   nfac = 1,
#'   lvmodel = "2PL"
#' )
#'
#' @export
makeDat <- function(N,R2Y,R2eta,omega,tau0,tau1,linear,ydist,lambda,nsec,nfac,lvmodel){

  mc <- match.call(expand.dots = TRUE)
  mc[[1L]] <- quote(list); # mc <- as.list(match.call()[-1])

  # set up S3 class
  sim_info <- structure(eval(mc), class = tolower(lvmodel))
  # sim_info<-structure(sim_condition,class=tolower(sim_condition$lvmodel))

  # Generate Latent Variable Model Information
  sim_info <- genLVinfo(sim_info = sim_info)

  # Generate True eta
  sim_info <- genTrueEta(Data = sim_info)

  # Generate LV part
  sim_info <- genLVM(info = sim_info)

  # simulate Y
  sim_info <- genOutcome(Data = sim_info)

  return(sim_info)
}


test_makeDat <- function(N,R2Y,R2eta,omega,tau0,tau1,linear,ydist,lambda,nsec,nfac,lvmodel){

  mc <- match.call(expand.dots = TRUE)
  mc[[1L]] <- quote(list); # mc <- as.list(match.call()[-1])

  # set up S3 class ---------------------------------------------------------
  sim_info <- structure(eval(mc), class = tolower(lvmodel))
  # sim_info<-structure(sim_condition,class=tolower(sim_condition$lvmodel))

  # Generate Latent Variable Model Information ------------------------------
  sim_info <- genLVinfo(sim_info = sim_info)

  # Generate True eta -------------------------------------------------------
  sim_info <- test_genTrueEta(Data = sim_info)

  # Generate LV part --------------------------------------------------------
  sim_info <- genLVM(info = sim_info)

  # simulate Y --------------------------------------------------------------
  sim_info <- test_genOutcome(Data = sim_info)

  return(sim_info)
}

#' Generate a matrix style data for simulation
#'
#' @description
#' \code{\link{makeInpDat}} is a function for generating a data based on
#' the given information.
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
#'   lvmodel = "sem"
#' )
#' @export
makeInpData <- function(N,R2Y,R2eta, omega,tau0,tau1,linear,ydist, lambda,nsec,nfac,lvmodel){

  mc <- match.call(expand.dots = TRUE)
  mc[[1L]] <- quote(list); # mc.list <- as.list(match.call(mc)[-1]))}

  # set up S3 class ---------------------------------------------------------
  sim_info <- structure(eval(mc), class = tolower(lvmodel))
  # sim_info <- structure(parsFromMod, class = tolower(parsFromMod$lvmodel))

  # Generate Latent Variable Model Information
  sim_info <- genLVinfo(sim_info = sim_info)

  # Generate True eta -------------------------------------------------------
  sim_info <- genTrueEta(sim_info)

  # Generate LV part --------------------------------------------------------
  sim_info <- genLVM(sim_info)

  d1 <- as.data.frame(matrix(rep(NA, nsec*(N/2)), ncol = nsec))
  s1 <- split(cbind(sim_info$studentM, sim_info$section)[,2], sim_info$studentM);
  for(i in 1:length(s1)) {
    # i = 1
    d1[i, s1[[i]]] <- sim_info$lv.resp[i, s1[[i]]]
    }
  d2 <- matrix(rep(NA, nsec*(N/2)), ncol = nsec)
  d3 <- rbind(d1, d2)
  names(d3) <- paste0("i", 1:ncol(d3))

  # simulate Y --------------------------------------------------------------
  sim_info <- genOutcome(sim_info)
  inp_data <- data.frame(Y = sim_info$stan_dt$Y,
                         Z = sim_info$stan_dt$Z,
                         sim_info$x, d3)

  list(
    sim_info = sim_info,
    inp_data = inp_data
  )
}
