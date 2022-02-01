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


detect_firstitem <- function(lambda_idx) {
  first_item <- apply(lambda_idx, 2, function(x) {which(x == 1)[1]})
  first_item_idx <- rep(0,nrow(lambda_idx))
  first_item_idx[first_item] <- 1
  first_item_idx
}
