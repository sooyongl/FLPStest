# setGeneric('makeFLPSdata',
#            function(x, ...)
#              standardGeneric("makeFLPSdata")
#            )
#
#
# setGeneric('makeFLPSmodel',
#            function(x, ...)
#              standardGeneric("makeFLPSmodel")
# )
#
#
#
#
# setMethod(f = "makeFLPSdata",
#           signature = "flpsData",
#           definition = function(.Object, ...) {
#
#             .Object <- callNextMethod()
#             .Object
#             }
#           )

# setClass("foo", representation(x = "numeric"))
# setClass("bar", contains = "foo")
#
# setGeneric("foobar", function(object, ...) standardGeneric("foobar"))
#
# setMethod("foobar", "foo", function(object, another.argument = FALSE, ...) {
#   print(paste("in foo-method:", another.argument))
#   if (another.argument) object@x^3
#   else object@x^2
# })
#
# setMethod("foobar", "bar", function(object, another.argument = FALSE, ...) {
#   print(paste("in bar-method:", another.argument))
#   object@x <- sqrt(object@x)
#   callNextMethod(object, another.argument, ...)
# })
# o1 <- new("bar", x = 4)
#
# foobar(o1, another.argument = TRUE)
