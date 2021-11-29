makeDat <- function(N,R2Y,omega,tau0,tau1,lambda,R2eta,nsec,lvmodel){
  
  lvmodel <- tolower(lvmodel)
  
  ## generate covariates ----------------------------------------------------
  
  ### Auxiliary Covariates
  x1 <- rnorm(N); #x1 <- x1 - mean(x1); x1 <- x1/sd(x1)
  x2 <- rnorm(N); #x2 <- x2 - mean(x2); x2 <- x2/sd(x2) # in proposal, it is binary.
  ### Treatment or Control
  Z <- rep(c(1,0),each=N/2)
  
  # Generate True eta -------------------------------------------------------
  ## generate etas
  # random.e <- rnorm(N,0,sqrt(1-R2eta))
  # eta <- sqrt(R2eta/2)*(x1-x2) + random.e
  
  eta <- rnorm(N, 0, 1)
  
  # random.e <- rnorm(N,0,sqrt(1))
  # eta <- 0*(x1-x2) + random.e
  
  # Generate LVM data -------------------------------------------------------
  info <- parsForLVM(theta = eta, nsec = nsec, data_type = lvmodel)
  grad <- generate(info); # methods(generate)
  
  lv.par <- grad$lv.par
  grad <- grad$resp
  original_data <- grad
  
  ## in the application we said the # obs/ student would be pois(lambda)
  ## it looks like a (descretized) version of the exponential fits the CTA1
  ## data much better (tho still not great)
  
  # nworked <- sample(1:nsec,N,replace=TRUE,prob=dexp(1:nsec,rate=1/lambda))
  nworked <- rep(nsec, N)
  
  ### this results in a somewhat lower mean than lambda
  
  # studentM <- do.call("c", lapply(seq(N/2),function(n) rep(n,each=nworked[n])))
  studentM <- do.call("c", lapply(seq(N),function(n) rep(n,each=nworked[n])))
  
  ## who works which section? Not sure how to do this right
  ## for now, just make it random. this is _not_ true for CT data
  # section <- do.call("c", lapply(seq(N / 2),
  #                                function(n) {
  #                                  sort(sample(1:nsec, nworked[n],
  #                                              replace = FALSE))}))
  
  section <- do.call("c", lapply(seq(N),
                                 function(n) {
                                   sort(sample(1:nsec, nworked[n],
                                               replace = FALSE))}))
  
  
  ss <- cbind(studentM, section)
  
  # d1 <- matrix(rep(NA, nsec*N), ncol = nsec)
  # s1 <- split(ss[,2], studentM);
  # for(i in 1:length(s1)) { d1[i, s1[[i]]] <- grad[i, s1[[i]]]  }
  
  grad <- sapply(1:dim(ss)[1], function(n) grad[ss[n,1], ss[n,2]] )
  
  ### simulate Y -------------------------------------------------------------
  random.Y <- rnorm(N,0,sqrt(1-R2Y))
  
  # Y <- sqrt(R2Y/2)*(x2-x1)+random.Y
  # Y <- Y + omega*eta
  # Y <- Y + Z*(tau0 + tau1*eta)
  
  Y <- tau0*Z + omega*eta + tau1*eta*Z + sqrt(R2Y/2)*(x2-x1) + random.Y
  # Y <- tau0*Z + omega*eta + tau1*eta*Z + 0*(x2-x1) + random.Y
  
  list(
    nsecWorked = length(section),
    nstud = N,
    nsec = nsec,
    lv.par = lv.par,
    studentM = studentM,
    section = section,
    grad = grad,
    X = cbind(x1,x2),
    ncov = 2,
    true_eta = eta,
    Z = Z,
    Y = Y,
    original_data = original_data
  )
}