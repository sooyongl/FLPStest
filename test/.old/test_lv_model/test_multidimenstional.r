rm(list = ls())
library(rstan); library(tidyverse)
for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)
source_funs <- ls()


# 2PL ---------------------------------------------------------------------
lvmodel <- "2pl"
ipar <- genIRTpar(20, ncat = 2, nfac = 2, lvmodel)
# eta <- MASS::mvrnorm(100, rep(0, 2), matrix(c(1,0,0,1),ncol=2))
eta <- MASS::mvrnorm(100, rep(0, 2), matrix(c(1,0,0,1),ncol=2))
dat <- generateLV.irt(lvmodel, eta, ipar)
colnames(dat) <- NULL

nitem <- nrow(ipar[grep("a",names(ipar))])
nfac  <- ncol(ipar[grep("a",names(ipar))])

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
a_idx <- gen_a_idx(nitem, nfac)

detect_firstitem <- function(lambda_idx) {
  first_item <- apply(lambda_idx, 2, function(x) {which(x == 1)[1]})
  first_item_idx <- rep(0,nrow(lambda_idx))
  first_item_idx[first_item] <- 1
  first_item_idx
}

fi_idx <- detect_firstitem(a_idx)

grad <- dat
idx <- which(!is.na(grad), arr.ind = T)

nsecWorked <- nrow(idx)
nstud <- nrow(grad)
nsec <- ncol(grad)

studentM <- idx[,1]
section <- idx[,2]

grad <- sapply(1:dim(idx)[1], function(n) grad[idx[n,1], idx[n,2]] )

dt.stan <- list(
  nsecWorked = nsecWorked,
  nsec  = nsec,
  nfac = nfac,
  nstud = nstud,
  section    = section,
  factoridx  = a_idx,
  firstitem = fi_idx,
  studentM  = studentM,
  grad = grad)

stan_model <- paste(read_lines("test/test_lv_model/stan/IRT_multivariate.stan"),
                    collapse = "\n")
cat(stan_model)
irt.stan <- rstan::stan(
  model_code = stan_model,
  data = dt.stan,
  iter = 4000,
  cores = 1,
  chains = 1
)

stan_model <- paste(read_lines("test/test_lv_model/stan/IRT.stan"),
                    collapse = "\n")
cat(stan_model)
irt2.stan <- rstan::stan(
  model_code = stan_model,
  data = dt.stan,
  iter = 4000,
  cores = 1,
  chains = 1
)

irt.stan <- as.data.frame(irt.stan)
irt2.stan <- as.data.frame(irt2.stan)

irt.stan %>%
  select(matches("diff\\[|disc\\[")) %>%
  summarise_all(mean) %>%
  matrix(., ncol = 2)

irt2.stan %>%
  select(matches("diff\\[|disc\\[")) %>%
  summarise_all(mean) %>%
  matrix(., ncol = 2)

# grm ---------------------------------------------------------------------
lvmodel <- "grm"
ipar <- genIRTpar(20, ncat = 3, 1, lvmodel)
# eta <- MASS::mvrnorm(100, rep(0, 2), matrix(c(1,0,0,1),ncol=2))
eta <- MASS::mvrnorm(100, rep(0, 1), matrix(c(1),ncol=1))
dat <- genIRTdt(lvmodel, eta, ipar)
dat <- dat + 1
colnames(dat) <- NULL

nitem <- nrow(ipar[grep("a",names(ipar))])
nfac  <- ncol(ipar[grep("a",names(ipar))])

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
a_idx <- gen_a_idx(nitem, nfac)

detect_firstitem <- function(lambda_idx) {
  first_item <- apply(lambda_idx, 2, function(x) {which(x == 1)[1]})
  first_item_idx <- rep(0,nrow(lambda_idx))
  first_item_idx[first_item] <- 1
  first_item_idx
}
fi_idx <- detect_firstitem(a_idx)

grad <- dat
idx <- which(!is.na(grad), arr.ind = T)

nsecWorked <- nrow(idx)
nstud <- nrow(grad)
nsec <- ncol(grad)

studentM <- idx[,1]
section <- idx[,2]

grad <- sapply(1:dim(idx)[1], function(n) grad[idx[n,1], idx[n,2]] )

dt.stan <- list(
  nsecWorked = nsecWorked,
  nsec  = nsec,
  nfac = nfac,
  nstud = nstud,
  max_k = max(dat),
  section    = section,
  factoridx  = a_idx,
  firstitem = fi_idx,
  studentM  = studentM,
  grad = grad)

stan_model <- paste(read_lines("test/test_lv_model/stan/GGRM_multivariate.stan"),
                    collapse = "\n")
cat(stan_model)
grm.stan <- rstan::stan(
  model_code = stan_model,
  data = dt.stan,
  iter = 4000,
  cores = 1,
  chains = 1
)

stan_model <- paste(read_lines("test/test_lv_model/stan/GGRM_ordered_long.stan"),
                    collapse = "\n")
cat(stan_model)
grm1.stan <- rstan::stan(
  model_code = stan_model,
  data = dt.stan,
  iter = 4000,
  cores = 1,
  chains = 1
)

grm.stan <- as.data.frame(grm.stan)
grm1.stan <- as.data.frame(grm1.stan)

grm.stan1 <- grm.stan %>%
  select(matches("diff\\[|disc\\[")) %>%
  summarise_all(mean)

grm.stan1 %>%
  select(matches("disc\\[")) %>%
  matrix(., ncol = 1)

grm.stan1 %>%
  select(matches("diff\\[")) %>%
  matrix(., ncol = 2)

grm2.stan1 <- grm1.stan %>%
  select(matches("ka\\[|alpha\\[")) %>%
  summarise_all(mean)

grm2.stan1 %>%
  select(matches("alpha\\[")) %>%
  matrix(., ncol = 1)

grm2.stan1 %>%
  select(matches("ka\\[")) %>%
  matrix(., ncol = 2)

# gpcm ---------------------------------------------------------------------
lvmodel <- "gpcm"
ipar <- genIRTpar(20, ncat = 3, 2, lvmodel)
eta <- MASS::mvrnorm(100, rep(0, 2), matrix(c(1,0,0,1),ncol=2))
dat <- genIRTdt(lvmodel, eta, ipar)
dat <- dat + 1
colnames(dat) <- NULL

nitem <- nrow(ipar[grep("a",names(ipar))])
nfac  <- ncol(ipar[grep("a",names(ipar))])

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
a_idx <- gen_a_idx(nitem, nfac)

detect_firstitem <- function(lambda_idx) {
  first_item <- apply(lambda_idx, 2, function(x) {which(x == 1)[1]})
  first_item_idx <- rep(0,nrow(lambda_idx))
  first_item_idx[first_item] <- 1
  first_item_idx
}
fi_idx <- detect_firstitem(a_idx)

grad <- dat
idx <- which(!is.na(grad), arr.ind = T)

nsecWorked <- nrow(idx)
nstud <- nrow(grad)
nsec <- ncol(grad)

studentM <- idx[,1]
section <- idx[,2]

grad <- sapply(1:dim(idx)[1], function(n) grad[idx[n,1], idx[n,2]] )

dt.stan <- list(
  nsecWorked = nsecWorked,
  nsec  = nsec,
  nfac = nfac,
  nstud = nstud,
  max_k = max(dat),
  section    = section,
  factoridx  = a_idx,
  firstitem = fi_idx,
  studentM  = studentM,
  grad = grad)

stan_model <- paste(read_lines("test/test_lv_model/stan/GPCM_multivariate.stan"),
                    collapse = "\n")
cat(stan_model)
gpcm.stan <- rstan::stan(
  model_code = stan_model,
  data = dt.stan,
  iter = 4000,
  cores = 1,
  chains = 1
)



# check_irt(dat, lvmodel, 2, IRTpars = F)
