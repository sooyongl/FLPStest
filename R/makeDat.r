## from table 1 in the grant application
## N is number of students ("sample size") make this even
## R2Y is Predictive power of covariates
## omega is "relationship between eta_T and Y_C
## tau0 is the treatment eff when eta_T=0
## tau1 is "relationship between eta_T and Y_T-Y_C
## (for now, single level (not multilevel), linear, normal)
## lambda mean Worked problems/person
## R2eta is "Predictive power of latent variable" (extent to which covariates predict eta)
## other parameters don't apply to rasch
makeDat <- function(N,R2Y,omega,tau0,tau1,lambda,R2eta,nsec, lvmodel){
  ## generate covariates
  x1 <- rnorm(N)
  x2 <- rnorm(N) # in proposal, it is binary.
  
  Z <- rep(c(1,0),each=N/2)
  

  # Generate True eta -------------------------------------------------------
  ## generate etas
  eta <- sqrt(R2eta/2)*(x1-x2) + rnorm(N,0,sqrt(1-R2eta))
  
  
  # Generate LVM data -------------------------------------------------------
  info <- parsForLVM(theta = eta, nsec = nsec, data_type = lvmodel)
  grad <- generate(info)
  
  ## in the application we said the # obs/ student would be pois(lambda)
  ## it looks like a (descretized) version of the exponential fits the CTA1
  ## data much better (tho still not great)
  nworked <- sample(1:nsec,N/2,replace=TRUE,prob=dexp(1:nsec,rate=1/lambda))
  ### this results in a somewhat lower mean than lambda
  
  studentM <- do.call("c", lapply(seq(N/2),function(n) rep(n,each=nworked[n])))
  
  ## who works which section? Not sure how to do this right
  ## for now, just make it random. this is _not_ true for CT data
  section <- do.call("c", lapply(seq(N / 2),function(n) {sort(sample(1:nsec, nworked[n], replace = FALSE))}))
  
  ss <- cbind(studentM, section)
  
  grad <- sapply(1:dim(ss)[1], function(n) grad[ss[n,1], ss[n,2]] )
  
  ### simulate Y
  Y <- sqrt(R2Y/2)*(x2-x1)+rnorm(N,0,sqrt(1-R2Y))
  Y <- Y+omega*eta
  Y <- Y+Z*(tau0*sd(Y)+tau1*sd(Y)*eta)
  
  list(
    nsecWorked=length(section),
    nstud=N,
    nsec=nsec,
    studentM=studentM,
    section=section,
    grad=grad,
    X=cbind(x1,x2),
    ncov=2,
    Z=Z,
    Y=Y
  )
}

