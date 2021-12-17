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
           flps_fit                 = "stanfit",
           time                     = "character"
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
           covariate       = "character",
           lv_type         = "character",
           lv_model        = "character",
           lv_data         = "list",
           stan_data       = "list"
         )
)

#' Class 'flpsIRT'
#'
#' \code{\linkS4class{flpsIRT}} is an S4 class
#'
setClass("flpsIRT",
         # slots = c(),
         contains = "flpsData"
)

#' Class 'flpsSEM'
#'
#' \code{\linkS4class{flpsSEM}} is an S4 class
#'
setClass("flpsSEM",
         # slots = c(),
         contains = "flpsData"
)

#' Class 'flpsLGM'
#'
#' \code{\linkS4class{flpsGPCM}} is an S4 class
#'
setClass("flpsGPCM",
         slots = c(
           time_loading = "numeric"
         ),
         contains = "flpsData"
)

#' Class 'flpsLGM'
#'
#' \code{\linkS4class{flpsLGM}} is an S4 class
#'
setClass("flpsLGM",
         slots = c(
           time_loading = "numeric"
           ),
         contains = "flpsData"
)

#' Class 'flpsMixture'
#'
#' \code{\linkS4class{flpsMixture}} is an S4 class
#'
setClass("flpsMixture",
         slots = c(
           nclass = "numeric"
         ),
         contains = "flpsData"
)
