#' @aliases show,flps-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "flps", function(object) {

  print(object)

  return(invisible(NULL))
})

#' @aliases print,flps-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "flps", function(x) {

  cat("This is a S4 class.")

  return(invisible(x))
})



#' @aliases show,flps-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "flpsData", function(object) {

  print(object)

  return(invisible(NULL))
})

#' @aliases print,flps-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "flpsData", function(x) {

  z0_mean <- round(mean(x@flps_data$Y [x@flps_data$Z == 0], na.rm = T),3)
  z1_mean <- round(mean(x@flps_data$Y [x@flps_data$Z == 1], na.rm = T),3)

  cat(paste("Z=0, mean_Y :", z0_mean,"Z=1, mean_Y :", z1_mean))

  return(invisible(x))
})
