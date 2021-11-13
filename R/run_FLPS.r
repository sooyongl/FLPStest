#' Conduct fully latent principal stratification
#'
#' @param inp_data a dataframe
#' @param flps_data a list
#' @export
runFLPS <- function(inp_data = NULL, flps_data = NULL, outcome, group, covariate, lv_model, lv_type, stan_options = list(...), ...) {


  # options -----------------------------------------------------------------
  .call <- match.call()
  .op <- stanOptions(stan_options)

  # import data -------------------------------------------------------------
  if(!is.null(inp_data) && !is.null(flps_data))
    stop("Data needed.")

  if(!is.null(inp_data)){
    flps_data <- makeFLPSdata(inp_data, outcome, group, covariate, lv_model, lv_type)
  } else {
    inp_data <- list()
  }

  # make FLPS model ---------------------------------------------------------
  # flps_model <- makeFLPSmodel()
  flps_model <- loadRstan(lv_model = lv_model)

  # fit FLPS ----------------------------------------------------------------
  flps_fit <- rstan::stan(
    data = flps_data,
    model_code = flps_model,
    model_name = .op$model_name,
    # file = .op$file,
    fit = .op$fit,
    pars = .op$pars,
    chains = .op$chains,
    iter = .op$iter,
    warmup = .op$warmup,
    thin = .op$thin,
    init = .op$init,
    cores = .op$cores,
    seed = .op$seed,
    algorithm = .op$algorithm,
    control = .op$control,
    sample_file = .op$sample_file,
    diagnostic_file = .op$diagnostic_file,
    save_dso = .op$save_dso,
    verbose = .op$verbose,
    include = .op$include,
    open_progress = .op$open_progress,
    boost_lib = .op$boost_lib,
    eigen_lib = .op$eigen_lib
  )

  o <- new("flps")

  o@call <- .call
  o@inp_data <- inp_data
  o@flps_data <- flps_data
  o@flps_fit <- flps_fit

  return(o)
}
