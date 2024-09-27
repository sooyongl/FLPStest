# .specifyIRT <- function(x) {
#
#   # need to run CFA or IRT to specify priors
#   # print("test")
#   outcome <- x@outcome
#   group <- x@group
#   covariate <- x@covariate
#   lv_type <- x@lv_type
#   lv_model <- x@lv_model
#   lv_data <- x@lv_data
#   stan_data <- x@stan_data
#
#   grad_max <- max(x@stan_data[["grad"]])
#   grad_min <- min(x@stan_data[["grad"]])
#
#   fs.prior.info <- apply(lv_data, 2, function(x) {
#     cor(x, rowMeans(lv_data, na.rm = T), use = "pairwise.complete.obs")
#   })
#
#   pos.sign <- which(fs.prior.info > 0)
#   neg.sign <- which(fs.prior.info < 0)
#   non.sign <- which(is.na(fs.prior.info))
#
#   lambda_prior <- ""
#   for(i in pos.sign){
#     lambda_prior <- glue::glue("{lambda_prior}\nlambda_free[{i}] ~ normal(1, 1);\n")
#   }
#   for(i in neg.sign){
#     lambda_prior <- glue::glue("{lambda_prior}\nlambda_free[{i}] ~ normal(-1, 1);\n")
#   }
#   for(i in non.sign){
#     lambda_prior <- glue::glue("{lambda_prior}\nlambda_free[{i}] ~ normal(0, 1);\n")
#   }
#
#
#   # stan.syntax <- glue::glue("{stan.data}\n{stan.parameters}\n{stan.trans.pram}\n{stan.model}\n")
#
#   stan.syntax <- "not yet"
#
#   return(stan.syntax)
# }
#
#
# .specifySEM <- function(x) {
#
#
#
# }
#
#
# .specifyLGM <- function(x) {
#
#
# }
#
#
# .specifyGPCM <- function(x) {
#
#
#
# }
#
# .specifyMixture <- function(x) {
#
#
#
# }
#
#
#
# # generic -----------------------------------------------------------------
# setGeneric('makeFLPSmodel',
#            function(x, ...)
#              standardGeneric("makeFLPSmodel")
# )
#
# setMethod("makeFLPSmodel", "flpsIRT",     .specifyIRT)
# setMethod("makeFLPSmodel", "flpsSEM",     .specifySEM)
# setMethod("makeFLPSmodel", "flpsGPCM",    .specifyGPCM)
# setMethod("makeFLPSmodel", "flpsLGM",     .specifyLGM)
# setMethod("makeFLPSmodel", "flpsMixture", .specifyMixture)
