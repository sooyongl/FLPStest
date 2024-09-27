#' make NULL S3 class
#' @noRd
S3class <- function(class) {
  out <- structure(list(), class = class)
  out
}

#' obtain the signs of factor loadings
#' @noRd
obv_lambda <- function(obs.v.partial, a_idx) {

  nsec <- nrow(a_idx)
  nfac <- ncol(a_idx)

  fs.prior.info <- apply(obs.v.partial, 2, function(x) {
    cor(x, rowMeans(obs.v.partial, na.rm = T), use = "pairwise.complete.obs")
  })

  fs.prior.info[which(fs.prior.info > 0)] <- 1
  fs.prior.info[which(fs.prior.info < 0)] <- -1
  fs.prior.info[which(is.na(fs.prior.info))] <- 0

  temp_idx <- apply(a_idx, 2, function(x) which(x == 1))

  a1 <- matrix(rep(0, nsec*nfac), ncol=nfac)
  for(x in 1:nfac) {
    a1[temp_idx[,x],x] <- fs.prior.info[temp_idx[,x]]

  }

  a1
}

#' @noRd
gen_a <- function(nitem, nfac) {
  idx_ <- rep(floor(nitem / nfac),nfac)
  idx_[length(idx_)] <- nitem - sum(idx_[-length(idx_)])
  idx_c <- c(0,cumsum(idx_))
  a    <- matrix(rep(0, nitem*nfac), ncol=nfac)
  a_idx <- matrix(rep(0, nitem*nfac), ncol=nfac)
  for(j in 1:nfac) { # j=1
    a_idx[(idx_c[j]+1):idx_c[(j+1)],j] <- 1
    # The first 1 here is the recommended constraint
    a[(idx_c[j]+1):idx_c[(j+1)],j] <- c(1, matrix(rlnorm((idx_[(j)]-1), .1, .3)))
  }
  colnames(a) <- paste0("a",1:ncol(a))

  list(a_idx = a_idx, a = a)
}

#' @noRd
gen_a_idx <- function(nitem, nfac) {
  idx_ <- rep(floor(nitem / nfac),nfac)
  idx_[length(idx_)] <- nitem - sum(idx_[-length(idx_)])
  idx_c <- c(0,cumsum(idx_))
  a    <- matrix(rep(0, nitem*nfac), ncol=nfac)
  a_idx <- matrix(rep(0, nitem*nfac), ncol=nfac)
  for(j in 1:nfac) { # j=1
    a_idx[(idx_c[j]+1):idx_c[(j+1)],j] <- 1
  }
  a_idx
}

#' @noRd
detect_firstitem <- function(lambda_idx) {
  first_item <- apply(lambda_idx, 2, function(x) {which(x == 1)[1]})
  first_item_idx <- rep(0,nrow(lambda_idx))
  first_item_idx[first_item] <- 1
  first_item_idx
}

#' @noRd
str_split_and <- function(x, char) {
  for(i in 1:length(char)) {
    x <- x[grepl(char[i], x)]
  }

  return(x)
}

#' Set the options of rstan
#' stanOptions(stan_options = list());
#' @noRd
stanOptions <- function(stan_options, ...) {

  # replace a missing value in a list -------------
  #
  # replaceMissing <- function(List, comp, value){
  #   arg <- paste(substitute(List), "$", substitute(comp), sep = "")
  #   arg.value <- eval(parse(text = arg), parent.frame(1))
  #   if (any(is.na(arg.value))) {
  #     change <- paste(arg, "<-", deparse(substitute(value)))
  #     a <- eval(parse(text = change), parent.frame(1))
  #   }
  #   invisible()
  # }

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
