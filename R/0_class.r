#' Class 'flps'
#'
#' \code{\linkS4class{flps}} is an S4 class
#'
setClass("flps",
  slots = c(
    call                     = "call",
    inp_data                 = "list", # "flpsData",
    flps_model               = "character",
    flps_data                = "flpsData",
    flps_fit                 = "stanfit"
    ),

  contains = character(),
  validity = function(object) {
    # if (!is.character(object@version) ) {
    #   stop("version ...")
    #   }
    return(TRUE)
    }
)

#' Initialize a flps class
#'
setMethod("initialize",
           signature  = "flps",
           definition = function(.Object) {
             .Object@flps_fit  <- new("stanfit", mode = 1L)
             .Object@flps_data <- new("flpsData")
             return(.Object)
})

#' Class 'flpsData'
#'
#' \code{\linkS4class{flpsData}} is an S4 class
#'
setClass("flpsData",
  slots = c(
    outcome         = "character",
    group           = "character",
    lv_type         = "character",
    lv_model        = "character",
    stan_data       = "list"
    ),

  contains = character(),

  validity = function(object) {
    return(TRUE)
  }
)

