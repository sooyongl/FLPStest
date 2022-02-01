#' make NULL S3 class
#'
S3class <- function(class) {
  out <- structure(list(), class = class)
  out
}

#' obtain the signs of factor loadings
#'
obv_lambda <- function(obs.v.partial, a_idx) {

  nsec <- nrow(a_idx)
  nfac <- ncol(a_idx)

  fs.prior.info <- apply(obs.v.partial, 2, function(x) {
    cor(x, rowMeans(obs.v.partial, na.rm = T), use = "pairwise.complete.obs")
  })

  fs.prior.info[which(fs.prior.info > 0)] <- 1
  fs.prior.info[which(fs.prior.info < 0)] <- -1
  fs.prior.info[which(is.na(fs.prior.info))] <- 0

  temp_idx <- apply(a_idx, 2, function(x) which(x == 1))

  a1 <- matrix(rep(0, nsec*nfac), ncol=nfac)
  for(x in 1:nfac) {
    a1[temp_idx[,x],x] <- fs.prior.info[temp_idx[,x]]

  }

  a1
}

# latent_labmda <- function(obs.v.partial, lv_type) {
# }

#' make stanmodel class
#'
# makeStanModel <- function(stan_code_path) {
#
#   stanfit <- rstan::stanc_builder(stan_code_path,
#                                   allow_undefined = TRUE,
#                                   obfuscate_model_name = FALSE)
#   stanfit$model_cpp <- list(model_cppname = stanfit$model_name,
#                             model_cppcode = stanfit$cppcode)
#   # create stanmodel object
#   sm <- methods::new(Class = "stanmodel",
#                      model_name = stanfit$model_name,
#                      model_code = stanfit$model_code,
#                      model_cpp = stanfit$model_cpp,
#                      mk_cppmodule = function(x) get(paste0("model_", model_name)))
#
#   sm
# }
