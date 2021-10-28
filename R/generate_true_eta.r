# -------------------------------------------------------------------------
# generate true eta (theta) -----------------------------------------------
# -------------------------------------------------------------------------
# generate eta (theta) in addition to existing values  ---------------------
genGivenValue <- function(given_value, mean_vec, cor_mat) {
  
  # stopifnot( length(mean_vec) == dim(cor_mat)[2] )
  # stopifnot( eigen( cor_mat )$values > 0 )
  
  # shift mean vectors to the target means
  shiftMean <- function(theta_mat, mean_vec){
    col_means <- colMeans(theta_mat)
    mean_centered <- theta_mat - matrix(rep(col_means, dim(theta_mat)[1]), nrow = dim(theta_mat)[1], byrow = T)
    theta_mat <- mean_centered + matrix(rep(mean_vec, dim(theta_mat)[1]), nrow = dim(theta_mat)[1], byrow = T)
    return(theta_mat)
  }
  
  n_col <- ifelse(is.null(dim(given_value)), 1, dim(given_value)[2])
  
  # Random data, with given_value
  n <- ifelse(is.null(dim(given_value)), length(given_value), dim(given_value)[1])
  k <- ncol(cor_mat)
  x <- matrix( rnorm(n*k), nc=k )
  x[,seq_len(n_col)] <- given_value
  
  # Rescale, first to make the variance equal to the identity matrix, 
  # then to get the desired correlation matrix.
  
  y <- x %*% solve(chol(var(x))) %*% chol(cor_mat) # cor(y)
  
  y[,seq_len(n_col)] <- given_value  # The first column was only rescaled: 
  
  # Rescale in terms of means
  y <- shiftMean(y, mean_vec) # cor(y); colMeans(y)
  
  return(y)
}

genTrueEta_try <- function(n_sample = 1000, r2_eta = 0.5, linear = T) {
  
  sig2_eta_tilde = 1 - r2_eta
  
  x1 = rnorm(n_sample, 0, 1)
  x1sq = x1^2
  x2 = rbinom(n_sample, 1, .5)
  
  if(linear){
    # linear
    # eta_tilde <- -x1 + 0.5*x2 + rnorm(n_sample, -mean(-x1 + 0.5*x2), sig_eta_tilde); var(eta_tilde)
    
    x_data <- cbind(x1, x2)
    beta = c(-1, 0.5)
    phi = cov(x_data)
    resi = (sig2_eta_tilde) - t(beta) %*% phi %*% beta
    
  } else {
    # non-linear
    # eta_tilde <- -x1 + 0.5*x1sq + 0.5*x2 + rnorm(n_sample, -mean(-x1 + 0.5*x1sq + 0.5*x2), sig_eta_tilde)
    
    x_data <- cbind(x1, x1sq, x2)
    beta = c(-1, 0.5, 0.5)
    phi = cov(x_data)
    resi = (sig2_eta_tilde) - t(beta) %*% phi %*% beta
  }
  
  mean_vec <- c(colMeans(x_data), beta %*% colMeans(x_data))
  cov_mat <- 
    rbind(
      cbind(phi, phi %*% beta),
      cbind(t(beta) %*% phi, t(beta) %*% phi %*% beta + resi)
    )
  cor_mat <- cor(cov_mat)
  
  eta_tilde <- genGivenValue(given_value = x_data, mean_vec, cor_mat)
  
  eta_true = eta_tilde / sqrt(sig2_eta_tilde) 
  
  data <- cbind(eta_tilde, eta_true[,3])
  data <- data.frame(data)# colMeans(data); var(eta_true)
  names(data) <- c("x1","x2", "eta_tilde","eta_true")
  
  
  return(data)
}

genTrueEta <- function(n_sample = 1000, r2_eta = 0.5, linear = T) {
  while(TRUE){
    data <- try(
      genTrueEta_try(n_sample, r2_eta, linear), 
      
      silent=TRUE)
    if(!is(data, 'try-error')) break
  }
  data
}

