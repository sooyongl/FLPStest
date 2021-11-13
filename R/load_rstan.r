#' Load rstan model
#'
#' Load rstan model
#'
#' @param lv_model a character specifying a latent model
#' @return An object of class \code{\linkS4class{stanmodel}}
#' @examples
#' stan_model <- rstan_path(lv_model = "rasch")
#' @export
loadRstan <- function(lv_model = "rasch") {

  # if(!dir.exists(file.path("inst", "stan")))
  #   stop("The stan code does not exist!")

  stan_path <- system.file("stan", package = "FLPS")
  stan_list <- list.files(stan_path)

  if(tolower(lv_model) %in% c("2pl", "3pl")) {
    lv_model <- "IRT"
  }

  stan_picked <- grep(toupper(lv_model), toupper(stan_list))
  stan_picked2 <- grep("stan", stan_list[stan_picked])
  stan_model <- stan_list[stan_picked][stan_picked2]

  stan_file <- file.path(stan_path, stan_model)
  stan_model <- paste(readLines(stan_file), collapse = "\n")

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


