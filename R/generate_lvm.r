# S3 generic for data generation ------------------------------------------
generate <- function(parsForLVM, ...) { 
  UseMethod("generate", parsForLVM)
}

# get information for data generation ready -------------------------------
parsForLVM <- function(..., data_type = "1PL") {
  
  info <- list(...)
  class(info) <- append(class(info), data_type)
  
  return(info)
}

## #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~# ##
##                    IRT model                    ##
## #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~# ##

generate.rasch <- function(.x, ...) {
  .Class <- "dich"
  NextMethod()
}

generate.1PL <- function(.x, ...) {
  .Class <- "dich"
  NextMethod()
}

generate.2PL <- function(.x, ...) {
  .Class <- "dich"
  NextMethod()
}

generate.3PL <- function(.x, ...) {
  .Class <- "dich"
  NextMethod()
}

# Dichotomous Response: Rasch, 1PL, 2PL, 3PL ------------------------------
generate.dich <- function(info, D = 1){
  # set up for data generation
  theta <- info$theta; nitem <- info$nsec
  
  nexaminee <- length(theta)
  nitem <- nitem
  
  if(inherits(info, "rasch")) {
    
    b <- rnorm(nitem) ## this is not based on original model
    a <- runif(nitem, 1, 1) ## this is not based on original model
    c <- runif(nitem,0,0) ## this is not based on original model
    
  } else if(inherits(info, "1PL")) {
    
    b <- rnorm(nitem) ## this is not based on original model
    a <- runif(nitem, 1, 1) ## this is not based on original model
    c <- runif(nitem,0,0) ## this is not based on original model
    
  } else if(inherits(info, "2PL")) {
    
    b <- rnorm(nitem) ## this is not based on original model
    a <- runif(nitem, 0.4, 1.2) ## this is not based on original model
    c <- runif(nitem,0,0) ## this is not based on original model
    
  } else if(inherits(info, "3PL")) {
    
    b <- rnorm(nitem) ## this is not based on original model
    a <- runif(nitem, 0.4, 1.2) ## this is not based on original model
    c <- runif(nitem,0,.2) ## this is not based on original model
    
  } else {
    
    b <- rnorm(nitem) ## this is not based on original model
    a <- runif(nitem, 1, 1) ## this is not based on original model
    c <- runif(nitem,0,0) ## this is not based on original model
    
  }
  
  # data generation
  pr <- matrix(NA, nexaminee, nitem)
  for (j in 1:nexaminee){
    pr[j,] <- c + (1 - c) / (1 + exp(-D * (a * (theta[j] - b))))
  }
  resp <- (matrix(runif(nexaminee*nitem), nexaminee, nitem) < pr) * 1
  
  return(resp)
}


# Polytomous Response: GPCM  ----------------------------------------------
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
  theta <- info$theta; ipar <- info$theta
  
  # data generation
  set.seed(seednum)
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
  
  return(resp)
}

generate.grm <- function(info) {
  print("not yet")
}


# Response Time (Lognormal)------------------------------------------------
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

## #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~# ##
##                    SEM model                    ##
## #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~# ##

# generate sem data -------------------------------------------------------
generate.sem <- function(info) {
  
  # set up for data generation
  theta <- info$theta; nitem <- info$nsec 
  
  loadings <- matrix(runif(nitem, 0.8, 1.2), ncol = 1)
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
  data <- latent + residuals
  
  return(data)
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










