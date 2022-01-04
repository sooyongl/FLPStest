.stanOptions <- list(
  file = NA,
  model_name = "flps_model",
  fit = NA,
  pars = NA,
  chains = 1,
  iter = 10000,
  warmup = 2000,
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
