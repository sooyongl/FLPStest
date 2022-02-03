#' Check simulated latent variable model parameters
#' @examples
#'
#' lvmodel <- "2pl"
#' ipar <- genIRTpar(20, ncat = 3, 2, lvmodel)
#' eta <- MASS::mvrnorm(100, rep(0, 2), matrix(c(1,0,0,1),ncol=2))
#' dat <- genIRTdt(lvmodel, eta, ipar)
#' check_irt(dat, lvmodel, 2, IRTpars = F)
#'
check_lv <- function(dat, covdata=NULL, lvmodel, nfac, IRTpars) {

  nitem <- ncol(dat);

  idx_ <- rep(floor(nitem / nfac),nfac)
  idx_[length(idx_)] <- nitem - sum(idx_[-length(idx_)])
  idx_c <- c(0,cumsum(idx_))

  mirt_model <- ""
  for(j in 1:nfac) { # j=1
    mirt_model <- paste(mirt_model, paste0("F", j, "=",(idx_c[j]+1),"-", idx_c[(j+1)]), sep = "\n")
  }

  lvmodel <- switch(lvmodel,
                    "rasch" = "Rasch",
                    "2pl" = "2PL",
                    "3pl" = "3PL",
                    "gpcm" = "gpcm",
                    "grm" = "graded",
                    lvmodel)

  cov.formula <- NULL
  if(!is.null(covdata)) {
    Xs <- names(covdata)
    cov.formula <- as.formula(paste0("~ ", paste(Xs, collapse = "+")))
  }

  res <- mirt::mirt(
    data = dat,
    model = mirt_model, # paste0("F = 1-",ncol(dat)),
    itemtype = lvmodel,
    covdata = covdata,
    formula = cov.formula,
    SE = F,
    verbose = FALSE
  )
  coef.res <- mirt::coef(res, IRTpars = IRTpars, simplify = T)
  items.res <- as.data.frame(coef.res$items)

  list(mirt.fit = res, items.est = items.res)
}


#' Check simulated FLPS part parameters
#'
check_flps <- function(stan_dt) {

  colnames(stan_dt$X) <- paste0("X", 1:ncol(stan_dt$X))
  # main effect ------------------------------------------
  true_data <- data.frame(Y = stan_dt$Y, stan_dt$X, eta = sdat$theta, Z = stan_dt$Z)

  etas <- names(true_data)[str_detect(names(true_data), "eta")]
  Xs <- names(true_data)[str_detect(names(true_data), "X")]

  etapart <- paste(etas, collapse = "+")
  etazpart <- paste(paste0(etas, "*Z"), collapse = "+")

  xpart <- paste(Xs, collapse = "+")

  true_form <- as.formula(glue::glue("Y ~ Z + {etapart} + {etazpart} + {xpart}"))
  all_to_y.fit <- lm(true_form, data = true_data)
  all_to_y <- coefficients(all_to_y.fit)

  x_to_factor <- c()
  for(i in 1:length(etas)) {
    f1 <- as.formula(glue::glue("{etas[i]} ~ {xpart}"))
    x_to_factor.fit <- lm(f1, data = true_data)
    coef.x <- coef(x_to_factor.fit)
    x_to_factor <- c(x_to_factor, coef.x[grepl("X", names(coef.x))])
  }

  omega <- paste0("a1", 1:length(etas))
  interterm <- paste0("b1", 1:length(etas))

  bux1 <- paste0("bu", 1:length(etas), "1")
  bux2 <- paste0("bu", 1:length(etas), "2")

  true_param <- c(all_to_y, x_to_factor)

  names(true_param) <- c("b00", "b0", omega, "by1","by2", interterm, bux1,bux2)
  true_param <- true_param[c("b00", "b0", interterm, omega,"by1","by2", bux1,bux2)]
  true_param
}
