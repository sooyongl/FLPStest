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

  samples <- x@flps_fit@sim$samples

  tau0 <- round(mean(unlist(lapply(samples, "[", "b0"))), 3)
  tau1 <- round(mean(unlist(lapply(samples, "[", "b1"))), 3)

  out <- paste0(
    paste("tau0 is", tau0),
    "\n",
    paste("tau1 is", tau1)
  )

  cat(out)

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

  out <- paste0(
    paste("LVM type:", x@lv_type, "\n"),
    paste("LVM syntax: ", x@lv_model, "\n"),
    paste("Z=0, mean_Y :", z0_mean,"Z=1, mean_Y :", z1_mean)
  )

  cat(out)

  return(invisible(x))
})
