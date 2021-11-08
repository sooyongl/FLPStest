#' Run FLPS Simulation
#'
#' Simulation of fully latent principal stratification
#'
#' @param pars a list of parameters
#' @return A list containing simulated data and stan fit
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
runSimulation <- function(pars, ...) {

  stan_data <- do.call("makeDat", pars)
  stan_model <- loadRstan(lv_model = pars$lvmodel)

  fit <- rstan::stan(
    model_code = stan_model@model_code,
    data = stan_data,
    # iter = 4000,
    # warmup = 1000,
    cores = 1,
    chains = 1
  )

  o <- list(pop_data = stan_data, stan_fit = fit)

  return(o)
}
