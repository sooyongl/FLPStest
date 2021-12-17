#' Conduct fully latent principal stratification
#'
#' @param inp_data A matrix
#' @param custom_data A list. should be provided with custome_stan
#' @param custom_stan A string. should be provided with custome_data
#' @param outcome A character indicating the name of an outcome variable
#' @param group A character indicating the name of a treatment/control group variable
#' @param covariate A character indicating the names of covariates variables
#' @param lv_model A description of the latent variable model, which is similar to the lavaan model syntax.
#' @param lv_type  A character indicating the type of latent variable models
#' @param stan_options A list containing \code{\link{stan}} options, using 'name = value'.
#' @param ... Additional options.
#' @return an object of class \code{flps}
#' @examples
#'
#' @export
runFLPS <- function(inp_data = NULL, custom_data = NULL, custom_stan = NULL, outcome, group, covariate, lv_model, lv_type, stan_options = list(...), ...) {

  start.time <- proc.time()[3L]

  # options -----------------------------------------------------------------
  .call <- match.call()
  argslist <- as.list(.call[-1])


  # validate ----------------------------------------------------------------
  if(!is.null(inp_data) && !is.null(custom_data))
    stop("Data is not provided.")

  if((!is.null(custom_data) && is.null(custom_stan))  | (is.null(custom_data) && !is.null(custom_stan)) )
    stop("Custom data and custome stan code must be provided at the same time!")

  # data and code -------------------------------------------------------------

  if(is.null(inp_data) && !is.null(custom_data) && !is.null(custom_stan)) {

    flps_data_class <- makeFLPSdata(custom_data, outcome, group, covariate, lv_model, lv_type, custom = T)
    flps_model <- custom_stan # loadRstan(lv_type) # cat(flps_model)

  }

  if (!is.null(inp_data) && is.null(custom_data)) {

    flps_data_class <- makeFLPSdata(inp_data, outcome, group, covariate, lv_model, lv_type, ...)
    flps_model <- makeFLPSmodel(x = flps_data_class)
  }

  # fit FLPS ----------------------------------------------------------------
  stan_options$data <- flps_data_class@stan_data
  stan_options$model_code <- flps_model

  if(!"iter" %in% names(stan_options)) {
    stan_options$iter <- 10000
    stan_options$warmup <- 2000
  }

  flps_fit <-  do.call(rstan::stan, stan_options)

  # class output ------------------------------------------------------------
  o <- new("flps")

  o@call       <- .call
  o@inp_data   <- inp_data
  o@flps_model <- flps_model
  o@flps_data  <- flps_data_class
  o@flps_fit   <- flps_fit

  o@time <- c("Timing:" = as.numeric(proc.time()[3L] - time$start.time))



  return(o)
}
