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
