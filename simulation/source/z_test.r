# latent_labmda <- function(obs.v.partial, lv_type) {
# }

#' make stanmodel class
#'
# makeStanModel <- function(stan_code_path) {
#
#   stanfit <- rstan::stanc_builder(stan_code_path,
#                                   allow_undefined = TRUE,
#                                   obfuscate_model_name = FALSE)
#   stanfit$model_cpp <- list(model_cppname = stanfit$model_name,
#                             model_cppcode = stanfit$cppcode)
#   # create stanmodel object
#   sm <- methods::new(Class = "stanmodel",
#                      model_name = stanfit$model_name,
#                      model_code = stanfit$model_code,
#                      model_cpp = stanfit$model_cpp,
#                      mk_cppmodule = function(x) get(paste0("model_", model_name)))
#
#   sm
# }

# Generate Binary data
#
# simData.dich <- function(a, d, guess, N, theta, D = 1){
#
#   # a = as.matrix(a),
#   # d = as.matrix(d),
#   # guess = as.vector(guess),
#   # N = N,
#   # theta = as.matrix(theta),
#   # itemtype = lvmodel
#
#   nexaminee <- N
#   nitem <- length(a)
#   g <- guess
#   # data generation
#   pr <- matrix(NA, nexaminee, nitem)
#   for (j in 1:nexaminee){
#     pr[j,] <- g + (1 - g) / (1 + exp(-D * (d + a * (theta[j]))))
#   }
#   resp <- (matrix(runif(nexaminee*nitem), nexaminee, nitem) < pr) * 1
#
#   # ipar <- data.frame(a = a, d = d, c = g)
#   return(resp)
# }

# Generate GPCM data (Under test)
#
# simData.gpcm <- function(a, d, N, theta){
#
#   disc <- a
#   loc <- d
#
#   K_j <- rep(ncol(d), nrow(d))
#   maxK <- max(K_j)
#
#   resp <- matrix(NA, nrow(theta), nitem)
#
#   for (i in 1:length(theta)){# i<-1
#
#     ### Compute item response category functions
#     pr <- matrix(NA, nitem, maxK + 1) # each row: P(X=0), ... ,P(X=K_j)
#     for (j in 1:nitem) { # j <- 1
#       exps_k <- rep(0, K_j[j]+1) # Exponentials at k = 0, ... , K_j
#       exps_k[1] <- exp(0)
#       for (k in 1:K_j[j]){ # h <- 1
#         exps_k[k+1] <- exp( k * disc[j] * theta[i] + sum(loc[j, 1:k]) )
#       }
#       pr[j, 1:(K_j[j]+1)] <- exps_k / sum(exps_k)
#     } # end of j
#
#     cumpr <- matrix(NA, nitem, maxK+1)
#     for (j in 1:nitem){ # j <- 1
#       for (k in 1:(K_j[j]+1)){# h <- 1
#         cumpr[j, k] <- sum(pr[j, 1:k])
#       }
#     }
#
#     tmp <- 1 * (cumpr >= matrix(rep(runif(nitem), maxK+1), nrow=nitem, ncol=maxK+1))
#     for (j in 1:nitem){ # j <- 1
#       if (sum(tmp[j,], na.rm=T)==(K_j[j]+1)){ # if all cumprs (including cpr_0) are larger than u
#         resp[i, j] <- 0
#       } else {
#         resp[i, j] <- min(which(tmp[j,]==1, arr.ind=T)) - 1
#       }
#     }
#
#   }
#
#   return(resp)
# }

# gelman diagnostic with chol.default error addressed.
# my.gelman.diag <- function(x,confidence = 0.95,transform = FALSE,autoburnin = FALSE,multivariate = TRUE) {
#   x <- as.mcmc.list(x)
#   if (nchain(x) < 2)
#     stop("You need at least two chains")
#   if (autoburnin && start(x) < end(x)/2)
#     x <- window(x, start = end(x)/2 + 1)
#   Niter <- niter(x)
#   Nchain <- nchain(x)
#   Nvar <- nvar(x)
#   xnames <- varnames(x)
#   if (transform)
#     x <- gelman.transform(x)
#   x <- lapply(x, as.matrix)
#   S2 <- array(sapply(x, var, simplify = TRUE),
#               dim = c(Nvar, Nvar, Nchain)
#   )
#   W <- apply(S2, c(1, 2), mean)
#   xbar <- matrix(sapply(x, apply, 2, mean, simplify = TRUE),
#                  nrow = Nvar, ncol = Nchain)
#   B <- Niter * var(t(xbar))
#   if (Nvar > 1 && multivariate) {  #ph-edits
#     # CW <- chol(W)
#     #    #This is W^-1*B.
#     # emax <- eigen(
#     #  backsolve(CW, t(backsolve(CW, B, transpose = TRUE)), transpose = TRUE),
#     # symmetric = TRUE, only.values = TRUE)$values[1]
#     emax <- 1
#     mpsrf <- sqrt((1 - 1/Niter) + (1 + 1/Nvar) * emax/Niter)
#   }  else {
#     mpsrf <- NULL
#   }
#
#   w <- diag(W)
#   b <- diag(B)
#   s2 <- matrix(apply(S2, 3, diag), nrow = Nvar, ncol = Nchain)
#   muhat <- apply(xbar, 1, mean)
#   var.w <- apply(s2, 1, var)/Nchain
#   var.b <- (2 * b^2)/(Nchain - 1)
#   cov.wb <- (Niter/Nchain) * diag(var(t(s2), t(xbar^2)) - 2 *
#                                     muhat * var(t(s2), t(xbar)))
#   V <- (Niter - 1) * w/Niter + (1 + 1/Nchain) * b/Niter
#   var.V <- ((Niter - 1)^2 * var.w + (1 + 1/Nchain)^2 * var.b +
#               2 * (Niter - 1) * (1 + 1/Nchain) * cov.wb)/Niter^2
#   df.V <- (2 * V^2)/var.V
#   df.adj <- (df.V + 3)/(df.V + 1)
#   B.df <- Nchain - 1
#   W.df <- (2 * w^2)/var.w
#   R2.fixed <- (Niter - 1)/Niter
#   R2.random <- (1 + 1/Nchain) * (1/Niter) * (b/w)
#   R2.estimate <- R2.fixed + R2.random
#   R2.upper <- R2.fixed + qf((1 + confidence)/2, B.df, W.df) *
#     R2.random
#   psrf <- cbind(sqrt(df.adj * R2.estimate), sqrt(df.adj * R2.upper))
#   dimnames(psrf) <- list(xnames, c("Point est.", "Upper C.I."))
#   out <- list(psrf = psrf, mpsrf = mpsrf, B = B, W = W) #added ph
#   class(out) <- "gelman.diag"
#   return( out )
# }


#  Check simulated latent variable model parameters
#
#  lvmodel <- "2pl"
#  ipar <- genIRTpar(20, ncat = 3, 2, lvmodel)
#  eta <- MASS::mvrnorm(100, rep(0, 2), matrix(c(1,0,0,1),ncol=2))
#  dat <- genIRTdt(lvmodel, eta, ipar)
#  check_irt(dat, lvmodel, 2, IRTpars = F)
#
# check_lv <- function(dat, covdata=NULL, lvmodel, nfac, IRTpars) {
#
#   nitem <- ncol(dat);
#
#   idx_ <- rep(floor(nitem / nfac),nfac)
#   idx_[length(idx_)] <- nitem - sum(idx_[-length(idx_)])
#   idx_c <- c(0,cumsum(idx_))
#
#   mirt_model <- ""
#   for(j in 1:nfac) { # j=1
#     mirt_model <- paste(mirt_model,
#                         paste0("F", j, "=",(idx_c[j]+1),"-", idx_c[(j+1)]),
#                         sep = "\n")
#   }
#
#   lvmodel <- switch(lvmodel,
#                     "rasch" = "Rasch",
#                     "2pl" = "2PL",
#                     "3pl" = "3PL",
#                     "gpcm" = "gpcm",
#                     "grm" = "graded",
#                     lvmodel)
#
#   cov.formula <- NULL
#   if(!is.null(covdata)) {
#     Xs <- names(covdata)
#     cov.formula <- as.formula(paste0("~ ", paste(Xs, collapse = "+")))
#   }
#
#   res <- mirt::mirt(
#     data = dat,
#     model = mirt_model, # paste0("F = 1-",ncol(dat)),
#     itemtype = lvmodel,
#     covdata = covdata,
#     formula = cov.formula,
#     SE = F,
#     verbose = FALSE
#   )
#   coef.res <- mirt::coef(res, IRTpars = IRTpars, simplify = T)
#   items.res <- as.data.frame(coef.res$items)
#
#   list(mirt.fit = res, items.est = items.res)
# }


# Check simulated FLPS part parameters
#
# check_flps <- function(stan_dt) {
#
#   colnames(stan_dt$X) <- paste0("X", 1:ncol(stan_dt$X))
#   # main effect ------------------------------------------
#   true_data <- data.frame(Y = stan_dt$Y, stan_dt$X, eta = sdat$theta, Z = stan_dt$Z)
#
#   etas <- names(true_data)[str_detect(names(true_data), "eta")]
#   Xs <- names(true_data)[str_detect(names(true_data), "X")]
#
#   etapart <- paste(etas, collapse = "+")
#   etazpart <- paste(paste0(etas, "*Z"), collapse = "+")
#
#   xpart <- paste(Xs, collapse = "+")
#
#   true_form <- as.formula(glue::glue("Y ~ Z + {etapart} + {etazpart} + {xpart}"))
#   all_to_y.fit <- lm(true_form, data = true_data)
#   all_to_y <- coefficients(all_to_y.fit)
#
#   x_to_factor <- c()
#   for(i in 1:length(etas)) {
#     f1 <- as.formula(glue::glue("{etas[i]} ~ {xpart}"))
#     x_to_factor.fit <- lm(f1, data = true_data)
#     coef.x <- coef(x_to_factor.fit)
#     x_to_factor <- c(x_to_factor, coef.x[grepl("X", names(coef.x))])
#   }
#
#   omega <- paste0("a1", 1:length(etas))
#   interterm <- paste0("b1", 1:length(etas))
#
#   bux1 <- paste0("bu", 1:length(etas), "1")
#   bux2 <- paste0("bu", 1:length(etas), "2")
#
#   true_param <- c(all_to_y, x_to_factor)
#
#   names(true_param) <- c("b00", "b0", omega, "by1","by2", interterm, bux1,bux2)
#   true_param <- true_param[c("b00", "b0", interterm, omega,"by1","by2", bux1,bux2)]
#   true_param
# }


