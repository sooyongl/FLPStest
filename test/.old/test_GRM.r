library(mirt)

genIRTpar <- function(nitem=20, ncat=2, lvmodel) {

  a <- c(1, matrix(rlnorm((nitem-1), .2, .3))) #the first 1 here is the recommended constraint

  if(lvmodel %in% c("grm","gpcm")) {

    if(lvmodel == "grm"){ # grm
      # for the graded model, ensure that there is enough space between the intercepts,
      # otherwise closer categories will not be selected often
      # (minimum distance of 0.3 here)
      diffs <- t(apply(matrix(runif(nitem * (ncat-1), .3, 1), nitem), 1, cumsum))
      d <- -(diffs - rowMeans(diffs))
    }
    else { # gpcm
        d <- matrix(rnorm(nitem * (ncat-1),), nitem)
        d[,1] <- 0 #the first 0 here is the recommended constraint for gpcm
    }

    colnames(d) <- paste0("d",1:ncol(d))

    ipar <- data.frame(a, d)

  }
  else if(lvmodel %in% c("rasch","2pl","3pl")) {

    g <- 0
    d <- rnorm(nitem)

    if(lvmodel == "rasch") {
      a <- 1
    }
    else if(lvmodel == "3pl") {
      g = runif(nitem, 0, 0.2)
    }

    ipar <- data.frame(a, d, g)
  }


  return(ipar)
}
genIRTpar(20, 4, "rasch")
genIRTpar(20, 4, "2pl")
genIRTpar(20, 4, "3pl")
genIRTpar(20, 4, "gpcm")
genIRTpar(20, 4, "grm")


genIRTdt <- function(lvmodel, eta, ipar) {

  stopifnot(is.data.frame(ipar))

  N    <- nrow(eta)
  nfac <- ncol(eta)

  lvmodel <- switch(lvmodel,
                    "rasch" = "dich",
                    "2pl" = "dich",
                    "3pl" = "dich",
                    "gpcm" = "gpcm",
                    "grm" = "graded")

  if(lvmodel == "dich") {
    a <- ipar["a"]
    d <- ipar["d"]
    guess <- ipar[,grep("g",names(ipar))]

  }

  if(lvmodel %in% c("gpcm","graded")) {
    a <- ipar[grep("a",names(ipar))]
    d <- ipar[grep("d",names(ipar))]
    guess <- 0
  }

  stopifnot(ncol(a) == ncol(eta))

  dat <- mirt::simdata(
    a = as.matrix(a),
    d = as.matrix(d),
    guess = as.vector(guess),
    N = N,
    Theta = as.matrix(eta),
    itemtype = lvmodel)

  return(dat)
}


genIRTdt(lvmodel="grm",
         eta = matrix(rnorm(100)),
         ipar = genIRTpar(20, 4, "grm"))



# test ---------------------------------------------------------------------

check_irt <- function(dat, lvmodel, IRTpars) {

  lvmodel <- switch(lvmodel,
                    "rasch" = "Rasch",
                    "2pl" = "2PL",
                    "3pl" = "3PL",
                    "gpcm" = "gpcm",
                    "grm" = "graded",
                    lvmodel)

  res <- mirt::mirt(
    data = dat,
    model = paste0("F = 1-",ncol(dat)),
    itemtype = lvmodel,
    SE = TRUE,
    verbose = FALSE
  )
  coef.res <- coef(res, IRTpars = IRTpars, simplify = TRUE)
  items.res <- as.data.frame(coef.res$items)
  items.res
}

irt.par <- genIRTpar(20, 4, "grm")
irt.dt <- genIRTdt(lvmodel="grm",
         eta = matrix(rnorm(100)),
         ipar = irt.par)

a1 <- check_irt(irt.dt, "grm", F)
a2 <- check_irt(irt.dt, "grm", T)

irt.par
a1
a2

irt.par <- genIRTpar(20, 4, "gpcm")
irt.dt <- genIRTdt(lvmodel="gpcm",
                   eta = matrix(rnorm(100)),
                   ipar = irt.par)

check_irt(irt.dt, "gpcm", IRTpars = F)




