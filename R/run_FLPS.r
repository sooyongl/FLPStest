#' Conduct fully latent principal stratification
#'
#' @param inp_data A matrix
#' @param flps_data A list
#' @param outcome A character indicating the name of an outcome variable
#' @param group A character indicating the name of a treatment/control group variable
#' @param covariate A character indicating the names of covariates variables
#' @param lv_model A description of the latent variable model, which is similar to the lavaan model syntax.
#' @param lv_type  A character indicating the type of latent variable models
#' @param stan_options A list containing \code{\link{stan}} options, using 'name = value'.
#' @param ... Additional options.
#' @examples
#'
#' @export
runFLPS <- function(inp_data = NULL, flps_data = NULL, outcome, group, covariate, lv_model, lv_type, stan_options = list(...), ...) {

  # options -----------------------------------------------------------------
  .call <- match.call()
  argslist <- as.list(.call[-1])

  # import data -------------------------------------------------------------
  if(!is.null(inp_data) && !is.null(flps_data))
    stop("Data is not provided.")

  if(!is.null(inp_data)){
    flps_data_class <- makeFLPSdata(inp_data, outcome, group, covariate, lv_model, lv_type)
    flps_data <- flps_data_class@flps_data
  } else {
    inp_data <- list()
  }

  # make FLPS model ---------------------------------------------------------
  flps_model <- loadRstan(lv_model = lv_type); # cat(flps_model)
  # flps_model<- makeFLPSmodel(flps_data_class)

  # fit FLPS ----------------------------------------------------------------
  stan_options$data <- flps_data
  stan_options$model_code <- flps_model

  flps_fit <-  do.call(rstan::stan, stan_options)

  # class output ------------------------------------------------------------
  o <- new("flps")

  o@call      <- .call
  o@inp_data  <- inp_data
  o@flps_model <- flps_model
  o@flps_data <- flps_data_class
  o@flps_fit  <- flps_fit

  return(o)
}
