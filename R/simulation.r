#' Run FLPS Simulation
#'
#' Simulation of fully latent principal stratification
#'
#' @param param_list a list of parameters
#' @param iter deault 2000
#' @param warmup default 1000
#' @param cores default 1
#' @param chains default 1
#' @return A list containing the parameter list, the simulated data and the stan fit
#' @details
#'
#' test a simulation.
#'
#' @examples
#' pars <- list(
#' N = 500,
#' R2Y = 0.2,
#' omega = 0.2,
#' tau0 = 0.13,
#' tau1 = -0.06,
#' lambda = 10,
#' R2eta = 0.2,
#' nsec = 20,
#' lvmodel = 'rasch'
#' )
#' \donttest{
#' runSimulation(pars)
#' }
#' @export runSimulation
runSimulation <- function(param_list, iter = 2000, warmup = 1000, cores = 1, chains = 1) {

  stan_data <- do.call("makeDat", param_list)
  stan_model <- loadRstan(lv_model = param_list$lvmodel)

  fit <- rstan::stan(
    model_code = stan_model@model_code,
    data = stan_data,
    iter = iter,
    warmup = warmup,
    cores = cores,
    chains = chains,
    pars = NA,
    include = TRUE
  )

  # sampling(
  # object = stan_model,
  # data = stan_data,
  # pars = pars,
  # init = init,
  # chains = chains,
  # iter = iter,
  # cores = cores,
  # verbose = verbose,
  # refresh = refresh,
  # ...)

  o <- list(pop_pars = param_list, pop_data = stan_data, stan_fit = fit)

  return(o)
}
