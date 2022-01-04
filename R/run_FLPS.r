#' Conduct fully latent principal stratification
#'
#' @param inp_data A matrix or a data frame
#' @param custom_data A list. should be provided with custome_stan
#' @param custom_stan A string. should be provided with custome_data
#' @param outcome A character indicating the name of an outcome variable
#' @param group A character indicating the name of a treatment/control group variable
#' @param covariate A character indicating the names of covariates variables
#' @param lv_model A description of the latent variable model, which is similar to the lavaan model syntax.
#' @param lv_type  A character indicating the type of latent variable models
#' @param stan_options A list containing \code{\link{stan}} options, using 'name = value'.
#' @param ... Additional arguments for latent variable models information (e.g., nclass = 2).
#' @return an object of class \code{flps}
#' @examples
#'
#' @export
runFLPS <- function(inp_data = NULL, custom_data = NULL, custom_stan = NULL, outcome, group, covariate, lv_model, lv_type, stan_options = list(), ...) {

  # time record ----------------------------------------------------------
  start.time <- proc.time()[3L]

  # call -----------------------------------------------------------------
  .call <- match.call()
  argslist <- as.list(.call[-1])

  # validate ----------------------------------------------------------------
  if(!is.null(inp_data) && !is.null(custom_data))
    stop("Data is not provided.")

  if((!is.null(custom_data) && is.null(custom_stan)) |
     (is.null(custom_data) && !is.null(custom_stan)))
    stop("Custom data and custome stan code must be provided at the same time!")

  # data and code -------------------------------------------------------------
  if(is.null(inp_data) && !is.null(custom_data) && !is.null(custom_stan)) {
    flps_data_class <- makeFLPSdata(custom_data, outcome, group, covariate, lv_model, lv_type, custom = T)
    flps_model <- custom_stan # loadRstan(lv_type) # cat(flps_model)
  }

  if (!is.null(inp_data) && is.null(custom_data)) {
    flps_data_class <- makeFLPSdata(inp_data, outcome, group, covariate, lv_model, lv_type, ...)

    # flps_model <- makeFLPSmodel(x = flps_data_class)
    flps_model <- loadRstan(flps_data_class@lv_type)
  }

  # fit FLPS ----------------------------------------------------------------
  if(class(flps_model) == "stanmodel") {
    stan_options <- stanOptions(stan_options, data = flps_data_class@stan_data, object = flps_model)
    flps_fit <-  do.call(rstan::sampling, stan_options)

  } else {
    stan_options <- stanOptions(stan_options, data = flps_data_class@stan_data, model_code = flps_model)
    flps_fit <-  do.call(rstan::stan, stan_options)
  }

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
