#' S3 generic for latent model data generation
#'
generate <- function(parsForLVM, ...) {
  UseMethod("generate", parsForLVM)
}

#' get information for data generation ready
#'
parsForLVM <- function(..., data_type = "1pl") {
  
  info <- list(...)
  class(info) <- append(class(info), data_type)
  
  return(info)
}

## #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~# ##
##                    IRT model                    ##
## #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~# ##

#' generate item pool
#'
genItemPool <- function(items = 20, nrCat = 4, model = "GPCM", same.nrCat = T){
  
  if(same.nrCat){
    gj <- rep(nrCat - 1, items)
  } else{
    gj <- rpois(items, nrCat - 1)
    gj[gj > nrCat - 1] <- nrCat - 1
    gj[gj < 1] <- 1
  }
  res <- matrix(NA, items, (max(gj) + 1))
  
  alphaj <- rlnorm(items, 0, 0.5)
  
  for (i in 1:items) {
    pars <- rnorm(gj[i], .111, 1.060)
    res[i, 1:(length(pars) + 1)] <- c(alphaj[i], pars)
  }
  
  if(model == "binary"){
    
    res <- cbind(res[,1],rowMeans(res[,2:nrCat-1])) %>% data.frame()
    name <- c("alphaj","beta")
    colnames(res) <- name
    
  } else {
    name <- c("a",paste("b", 1:(ncol(res)-1)))
    colnames(res) <- name
    res <- data.frame(res, K_j = gj)
  }
  return(res)
}

# getResponse <- function(th, it, model="GPCM",  D = 1.7) {
#   if(model == "binary") {
#     it <- it[,1:2]
#     pr <- (1/(1+exp(-D*it[,1]*(th-it[,2]))))
#     RES <- ifelse(pr > runif(1,0,1), 1, 0)
#   } else {
#     it <- it[, 1:(ncol(it)-1)]
#
#     nc <- ncol(it)
#     prov <- prov1 <- prov2 <- prov3 <- matrix(NA, nrow(it),nc)
#
#     dj <- v <- 0
#
#     for (t in 1:(ncol(it) - 1)) {
#       dj <- c(dj, dj[t] + it[1, 1] * D * (th - it[1, t + 1]))
#       v <- c(v, it[1, 1] * t)
#     }
#
#     v <- v[!is.na(dj)]
#     dj <- dj[!is.na(dj)]
#     Gammaj <- exp(dj)
#     dGammaj <- Gammaj * v
#     d2Gammaj <- Gammaj * v^2
#     d3Gammaj <- Gammaj * v^3
#     Sg <- sum(Gammaj)
#     Sdg <- sum(dGammaj)
#     Sd2g <- sum(d2Gammaj)
#     Sd3g <- sum(d3Gammaj)
#     n <- length(Gammaj)
#     prov[1, 1:n] <- Gammaj/Sg
#     prov1[1, 1:n] <- dGammaj/Sg - Gammaj * Sdg/Sg^2
#     prov2[1, 1:n] <- d2Gammaj/Sg - 2 * dGammaj * Sdg/Sg^2 - Gammaj * Sd2g/Sg^2 + 2 * Gammaj * Sdg^2/Sg^3
#     prov3[1, 1:n] <- d3Gammaj/Sg - (Gammaj * Sd3g + 3 * dGammaj * Sd2g + 3 * d2Gammaj * Sdg)/Sg^2 + (6 * Gammaj * Sdg * Sd2g + 6 * dGammaj * Sdg^2)/Sg^3 - 6 * Gammaj * Sdg^3/Sg^4
#
#     res <- list(Pi = prov, dPi = prov1, d2Pi = prov2, d3Pi = prov3)
#
#     pr <- res$Pi
#     RES <- rep(NA, nrow(pr))
#     for (i in 1:nrow(pr)) {
#       pp <- pr[i, ][!is.na(pr[i, ])]
#       vec <- rmultinom(n = 1, size = 1, prob = pp)
#       RES[i] <- (1:nrow(vec))[vec[, 1] == 1] - 1
#     }
#   }
#   list(prob = pr, RES = RES)
# }

#' methods for rasch model
#'
generate.rasch <- function(.x, ...) {.Class <- "dich"; NextMethod()}

#' method for 1PL model
#'
generate.1pl   <- function(.x, ...) {.Class <- "dich"; NextMethod()}

#' method for 2PL model
#'
generate.2pl   <- function(.x, ...) {.Class <- "dich"; NextMethod()}

#' method for 3PL model
#'
generate.3pl   <- function(.x, ...) {.Class <- "dich"; NextMethod()}

#' method for all dichotomous IRT model
#'
generate.dich <- function(info, D = 1){
  # set up for data generation
  theta <- info$theta; nitem <- info$nsec
  
  nexaminee <- length(theta)
  nitem <- nitem
  
  if(inherits(info, "rasch")) {
    
    ni <- nitem/5
    
    b <- rnorm(nitem) ## this is not based on original model
    b <- c(rep(-0.3, ni),rep(-0.1, ni),rep(0, ni),rep(0.4, ni),rep(0.6, ni))
    a <- runif(nitem, 1, 1) ## this is not based on original model
    c <- runif(nitem,0,0) ## this is not based on original model
    
  } else if(inherits(info, "1pl")) {
    
    b <- rnorm(nitem) ## this is not based on original model
    a <- runif(nitem, 1, 1) ## this is not based on original model
    c <- runif(nitem,0,0) ## this is not based on original model
    
  } else if(inherits(info, "2pl")) {
    
    b <- rnorm(nitem) ## this is not based on original model
    a <- rlnorm(nitem, 0, 0.5) ## this is not based on original model
    c <- runif(nitem,0,0) ## this is not based on original model
    
  } else if(inherits(info, "3pl")) {
    
    b <- rnorm(nitem) ## this is not based on original model
    a <- rlnorm(nitem, 0, 0.5) ## this is not based on original model
    c <- runif(nitem,0,.2) ## this is not based on original model
    
  } else {
    
    b <- rnorm(nitem) ## this is not based on original model
    a <- rlnorm(nitem, 0, 0.5) ## this is not based on original model
    c <- runif(nitem,0,0) ## this is not based on original model
    
  }
  
  # data generation
  pr <- matrix(NA, nexaminee, nitem)
  for (j in 1:nexaminee){
    pr[j,] <- c + (1 - c) / (1 + exp(-D * (a * (theta[j] - b))))
  }
  resp <- (matrix(runif(nexaminee*nitem), nexaminee, nitem) < pr) * 1
  
  ipar <- data.frame(a = a, b = b, c = c)
  return(list(resp = resp, lv.par = ipar))
}


#' method for Polytomous Response: GPCM
#'
generate.gpcm <- function(info){
  ## Generate responses for multiple people to multiple items
  
  ## Pr(X_{ij} = k | \theta_i)
  ##    \propto \exp (  \sum_{l=0}^k (a_j \theta_i + b_{jl} ) )
  
  ## 'ipar' Need to have labels, ("a", "b1", ... "bK", "K_j")
  
  ## | Debug ---------------
  # theta <- theta[i]; # i <- 6
  # ipar = rbind(diff);
  # ipar <- ipar[1:nitemtl,]
  # ipar <- pool[iset,2:(maxitemsc+2)]
  ## ------------------------|
  
  # set up for data generation
  theta <- info$theta
  
  ipar <- genItemPool(info$nsec, 4, "GPCM", T)
  
  # getResponse(theta[1], ipar[1,], model=model,  D = 1.7)
  
  # data generation
  # set.seed(seednum)
  nitem <- dim(ipar)[1]
  disc <- ipar[,"a"]
  loc <- ipar[,grep("b", colnames(ipar))]
  K_j <- ipar[,"K_j"]
  maxK <- max(K_j)
  
  resp <- matrix(NA, length(theta), nitem)
  
  for (i in 1:length(theta)){# i<-1
    
    ### Compute item response category functions
    pr <- matrix(NA, nitem, maxK + 1) # each row: P(X=0), ... ,P(X=K_j)
    for (j in 1:nitem) { # j <- 1
      exps_k <- rep(0, K_j[j]+1) # Exponentials at k = 0, ... , K_j
      exps_k[1] <- exp(0)
      for (k in 1:K_j[j]){ # h <- 1
        exps_k[k+1] <- exp( k * disc[j] * theta[i] + sum(loc[j, 1:k]) )
      }
      pr[j, 1:(K_j[j]+1)] <- exps_k / sum(exps_k)
    } # end of j
    
    cumpr <- matrix(NA, nitem, maxK+1)
    for (j in 1:nitem){ # j <- 1
      for (k in 1:(K_j[j]+1)){# h <- 1
        cumpr[j, k] <- sum(pr[j, 1:k])
      }
    }
    
    tmp <- 1 * (cumpr >= matrix(rep(runif(nitem), maxK+1), nrow=nitem, ncol=maxK+1))
    for (j in 1:nitem){ # j <- 1
      if (sum(tmp[j,], na.rm=T)==(K_j[j]+1)){ # if all cumprs (including cpr_0) are larger than u
        resp[i, j] <- 0
      } else {
        resp[i, j] <- min(which(tmp[j,]==1, arr.ind=T)) - 1
      }
    }
    
  }
  
  return(list(resp = resp + 1, lv.par = ipar))
}

#' method for Polytomous Response: GRM
#'
generate.grm <- function(info) {
  print("not yet")
}

#' method for Polytomous Response: NRM
#'
generate.nominal <- function(info) {
  print("not yet")
}

#' method for Polytomous Response: Response Time (Lognormal)
#'
generate.ln <- function(info){
  # set up for data generation
  tau <- info$tau; ipar <- info$ipar
  
  # data generation
  nexaminee <- length(tau)
  nitem <- nrow(ipar)
  alp <- ipar[,"alp"]; bet <- ipar[,"bet"]
  
  retime <- matrix(NA, nexaminee, nitem)
  for (j in 1:nexaminee){
    retime[j,] <- rlnorm(nitem, bet-tau[j], 1/alp)
    #m <- bet - tau[j]
    #s <- 1/alp
    #logmu <- log(m^2 / sqrt(s^2 + m^2))
    #logsd <- sqrt(log(1 + (s^2 / m^2)))
  }
  return(retime)
}

#' method for genderating sem data
#'
generate.sem <- function(info) {
  
  # set up for data generation
  theta <- info$theta; nitem <- info$nsec
  
  loadings <- matrix(runif(nitem, 0.8, 1.2), ncol = 1)
  loadings[1] <- 1
  residuals <- diag(1, nitem)
  
  if(is.null(dim(theta))) {
    n_sample <- length(theta)
    theta <- matrix(theta)
    
  } else {
    n_sample <- dim(theta)[1]
  }
  
  residuals <- MASS::mvrnorm(n = n_sample,
                             mu = rep(0, nrow(loadings)),
                             Sigma = residuals)
  
  # data generation
  latent <- tcrossprod(theta, loadings)
  resp <- latent + residuals
  
  return(list(resp = resp, lv.par = loadings))
  
}

## #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~# ##
##                   Mixture model                 ##
## #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~# ##

class_assign <- function(...) {
  eta <- list(...)
  eta$ref <- rep(0, length(eta[[1]]))
  eta <- lapply(eta, exp)
  eta <- do.call("cbind", eta)
  
  sum_eta <- apply(eta, 1, function(x) Reduce(sum, x))
  clasS_prob <- apply(eta, 2, function(x) x / (sum_eta))
  
  assignment <- apply(clasS_prob, 1, function(x) { (dim(clasS_prob)[2] + 1) - which.max(x)})
  
  assignment
}

# generate lca data ---------------------------------------------------
generate.lca <- function(info) {
  
  n_class <- info$n_class
  n_indi <- info$n_indi
  theta <- info$theta
  seperation <- info$seperation
  seperation <- c(seperation, 1- seperation)
  
  latent_class <- class_assign(theta)
  class_prop <- table(latent_class)
  
  # Measurement models within classes
  data <- lapply(1:length(class_prop), function(i) {
    
    class_n_size <- class_prop[i]
    
    row_data <- mvtnorm::rmvnorm(n = class_n_size,
                                 mean = rep(0, n_indi),
                                 sigma = diag(n_indi))
    
    continuousData <- row_data
    threshold = c(1 - seperation[i])
    
    quants <- sapply(1:(dim(continuousData)[2]), function(i) {
      quantile(continuousData[,i], probs = threshold[1])
    })
    
    ordinalData <- sapply(1:n_indi, function(i) {
      as.numeric(cut(as.vector(continuousData[,i]),c(-Inf,quants[i],Inf)))
    })
    
    useData <- data.frame(ordinalData)
    names(useData) <- paste0("x", 1:dim(useData)[2])
    
    useData$class <- i
    
    useData
  })
  
  data <- do.call('rbind', data)
  
  return(data)
  
}

# generate lpa data ---------------------------------------------------
generate.lpa <- function(info) {
  
  n_class <- info$n_class
  n_indi <- info$n_indi
  theta <- info$theta
  mean_list <- info$mean_list
  # seperation <- c(seperation, 1- seperation)
  
  latent_class <- class_assign(theta)
  class_prop <- table(latent_class)
  #
  # # Measurement models within classes
  data <- lapply(1:length(class_prop), function(i) {
    
    class_n_size <- class_prop[i]
    
    row_data <- mvtnorm::rmvnorm(n = class_n_size,
                                 mean = mean_list[[i]],
                                 sigma = diag(n_indi))
    
    useData <- data.frame(row_data)
    
    useData$class <- i
    names(useData) <- paste0("x", 1:dim(useData)[2], "class")
    useData
  })
  #
  data <- do.call('rbind', data)
  #
  return(data)
  
}

# generate general mixture data -------------------------------------------
generate.mixture <- function(info) {
  
  # return(data)
  print("not yet")
}










