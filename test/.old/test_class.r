

setClass("flpstest1",
         slots = c(
           outcome         = "character"
         ),
)

setClass("flpstest2",
         slots = c(
           ggggggg = "numeric"),
         contains = "flpstest1"
)

setClass("flpstest3",
         slots = c(
           dddddd = "flpstest1")
)


a0 <- new("flpstest1")
a1 <- new("flpstest2")

a2 <- new("flpstest3")

a2@dddddd <- a0


.specifymodel1 <- function(x) {print("dd")}

setGeneric('testGen', function(x) standardGeneric("testGen"))

setMethod("testGen",
          signature(x = "flpstest3"),
          .specifymodel1)

