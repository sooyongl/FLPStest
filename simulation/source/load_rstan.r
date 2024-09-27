#' Load rstan model
#'
#' Load rstan model
#'
#' @param lv_model a character specifying a latent model
#' @return An object of class \code{\linkS4class{stanmodel}}
#' @examples
#' stan_model <- rstan_path(lv_type = "rasch")
# #' @export
loadRstan <- function(lv_type = "2PL") {

  # if(!dir.exists(file.path("inst", "stan")))
  #   stop("The stan code does not exist!")


  if(tolower(lv_type) %in% c("rasch","2pl", "3pl")) {
    lv_type <- "IRT"
  }

  # stan_path <- system.file("rds", package = "FLPS")
  # stan_path <- system.file("stan", package = "FLPS")
  stan_path <- "inst/stan"
  stan_list <- list.files(stan_path)
  # stan_list <- list.files("inst/stan")

  if(tolower(lv_type) != "lca") {
    stan_list <- stan_list[grepl(toupper("flps"), toupper(stan_list))]
  }

  stan_picked <- grepl("\\.rds", stan_list)
  stan_picked1 <- stan_list[stan_picked]

  stan_picked <- grepl(toupper(lv_type), toupper(stan_picked1))
  stan_model <- stan_picked1[stan_picked]

  stan_file <- file.path(stan_path, stan_model)

  stan_model <- readRDS(stan_file)
  # stan_model <- paste(readLines(stan_file), collapse = "\n")

  return(stan_model)

  # stanfit <- rstan::stanc_builder(stan_file,
  #                                 allow_undefined = TRUE,
  #                                 obfuscate_model_name = FALSE)
  # stanfit$model_cpp <- list(model_cppname = stanfit$model_name,
  #                           model_cppcode = stanfit$cppcode)
  # # create stanmodel object
  # methods::new(Class = "stanmodel",
  #              model_name = stanfit$model_name,
  #              model_code = stanfit$model_code,
  #              model_cpp = stanfit$model_cpp,
  #              mk_cppmodule = function(x) get(paste0("model_", model_name)))
}


