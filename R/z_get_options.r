#' Set the options of rstan
#'
#' stanOptions(stan_options = list());
stanOptions <- function(stan_options, ...) {

  dots <- list(...)

  if(!"chain" %in% names(stan_options)) {
    stan_options$chain <- 1
  }

  if(!"iter" %in% names(stan_options)) {
    stan_options$iter <- 10000
    stan_options$warmup <- 2000

  } else {

    if(!"warmup" %in% names(stan_options)) {
      stan_options$warmup <- floor(stan_options$iter / 2)
    }
  }

  for(i in 1:length(dots)){
    stan_options[[names(dots)[i]]] <- dots[[i]]
  }

  # if (sys.parent() == 0)
  # e <- asNamespace("FLPS")
  # else
  # e <- parent.frame()
  # if(length(stan_options) > 0) {
  #   for(i in 1:length(stan_options)){
  #     .stanOptions[[which(names(.stanOptions) %in% names(stan_options)[i])]] <-
  #       stan_options[[i]]
  #   }
  # }

  return(stan_options)
}


#' replace a missing value in a list
#'
replaceMissing <- function(List, comp, value){
  arg <- paste(substitute(List), "$", substitute(comp), sep = "")
  arg.value <- eval(parse(text = arg), parent.frame(1))
  if (any(is.na(arg.value))) {
    change <- paste(arg, "<-", deparse(substitute(value)))
    a <- eval(parse(text = change), parent.frame(1))
  }
  invisible()
}
