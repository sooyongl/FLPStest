.stanOptions <- list(
  file = NA,
  model_name = "flps_model",
  fit = NA,
  pars = NA,
  chains = 4,
  iter = 2000,
  thin = 1,
  init = "random",
  seed = sample.int(.Machine$integer.max, 1),
  algorithm = c("NUTS","HMC", "Fixed_param"),
  control = NULL,
  sample_file = NULL,
  diagnostic_file = NULL,
  save_dso = TRUE,
  verbose = FALSE,
  include = TRUE,
  cores = getOption("mc.cores", 1L),
  open_progress = interactive() &&!isatty(stdout()) && !identical(Sys.getenv("RSTUDIO"),"1"),
  boost_lib = NULL,
  eigen_lib = NULL
)

# .op <- stanOptions(stan_options)
# flps_fit <- rstan::stan(
#   data = flps_data,
#   model_code = flps_model,
#   model_name = .op$model_name,
#   # file = .op$file,
#   fit = .op$fit,
#   pars = .op$pars,
#   chains = .op$chains,
#   iter = .op$iter,
#   warmup = .op$warmup,
#   thin = .op$thin,
#   init = .op$init,
#   cores = .op$cores,
#   seed = .op$seed,
#   algorithm = .op$algorithm,
#   control = .op$control,
#   sample_file = .op$sample_file,
#   diagnostic_file = .op$diagnostic_file,
#   save_dso = .op$save_dso,
#   verbose = .op$verbose,
#   include = .op$include,
#   open_progress = .op$open_progress,
#   boost_lib = .op$boost_lib,
#   eigen_lib = .op$eigen_lib
# )

.onAttach <- function(libname, pkgname) {

  if (packageVersion("rstan") < "2.8.0") {
    stop("Install the latest version of rstan")
  }

  print("being tested")
  
  # version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
  # packageStartupMessage("This is ", paste(pkgname, version), "\n", pkgname, " is a demo.")

}

# .onLoad <- function(...) {
#     directoryPath = system.file("", package = "FLPS")
# }
