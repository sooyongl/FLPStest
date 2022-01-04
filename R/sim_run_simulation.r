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
runSimulation <- function(param_list, iter = 2000, warmup = 1000, cores = 1, chains = 1, pars = NA) {
#
#   # print("It is deprecated. Use runFLPS instead.")
#   .call <- match.call()
#
#   # generate data -----------------------------------------------------------
#   stan_data <- do.call("makeDat", param_list)
#
#   # generate stan code ------------------------------------------------------
#   stan_model <- loadRstan(lv_model = param_list$lvmodel)
#
#   # fit FLPS ----------------------------------------------------------------
#   fit <- rstan::stan(
#     model_code = stan_model,
#     data = stan_data,
#     iter = iter,
#     warmup = warmup,
#     cores = cores,
#     chains = chains,
#     pars = pars
#   )
#
#   # output ------------------------------------------------------------------
#   o <- new("flps")
#
#   o@call         <- .call
#   o@param_info   <- param_list
#   o@flps_data    <- stan_data
#   o@flps_model   <- stan_model
#   o@flps_fit     <- fit
#
#   o <- list(pop_pars = param_list, pop_data = stan_data, stan_fit = fit)
#   #
#   return(o)
}
