.onAttach <- function(libname, pkgname) {
  if (packageVersion("rstan") < "2.8.0") {
    stop("Install the latest version of rstan")
  }
}

# .onLoad <- function(...) {
#     directoryPath = system.file("", package = "FLPS")
# }
