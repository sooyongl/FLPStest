obv_lambda <- function(obs.v.partial) {
  fs.prior.info <- apply(obs.v.partial, 2, function(x) {
    cor(x, rowMeans(obs.v.partial, na.rm = T), use = "pairwise.complete.obs")
  })

  fs.prior.info[which(fs.prior.info > 0)] <- 1
  fs.prior.info[which(fs.prior.info < 0)] <- -1
  fs.prior.info[which(is.na(fs.prior.info))] <- 0

  fs.prior.info
}

latent_labmda <- function(obs.v.partial, lv_type) {



}
