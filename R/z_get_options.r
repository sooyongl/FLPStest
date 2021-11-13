#' Set the options of rstan
#'
#' stanOptions(stan_options = list());
stanOptions <- function(stan_options) {

  .stanOptions$warmup <- floor(.stanOptions$iter / 2)

  # if (sys.parent() == 0)
  e <- asNamespace("FLPS")
  # else
  # e <- parent.frame()
  if(length(stan_options) > 0) {
    for(i in 1:length(stan_options)){
      .stanOptions[[which(names(.stanOptions) %in% names(stan_options)[i])]] <-
        stan_options[[i]]
    }
  }
  .stanOptions$warmup <- floor(.stanOptions$iter / 2)

  # for(i in 1:length(.stanOptions)) {
  #   assign(names(.stanOptions)[i], .stanOptions[[i]], envir = e)
  # }
  # chain_id, init_r, test_grad, append_samples, refresh, enable_random_init
  return(.stanOptions)
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
