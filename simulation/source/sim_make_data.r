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

