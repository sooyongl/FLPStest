library(tidyverse)



parsFromMod <- list(
    N=5308,
    R2Y=0.2, ## from app
    omega=0.2,
    tau0=0.13, ## from paper
    tau1=0.06, ## from paper "a difference of one IQR in etaT was associated with a reduction of 0.083 in the effect size" 0.083/1.35~~ 0.06
    lambda=43, ## from data used in model
    R2eta=0.5, ## from app
    nsec=134 ## from data used in model
)

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
makeRaschDat <- function(N,R2Y,omega,tau0,tau1,lambda,R2eta,nsec){
    ## generate covariates
    x1 <- rnorm(N)
    x2 <- rnorm(N)

    Z <- rep(c(1,0),each=N/2)

    ## generate etas
    eta <- sqrt(R2eta/2)*(x1-x2)+rnorm(N,0,sqrt(1-R2eta))

    ## section "difficulty" parameters
    secDiff <- rnorm(nsec) ## this is not based on original model

    ## in the application we said the # obs/ student would be pois(lambda)
    ## it looks like a (descretized) version of the exponential fits the CTA1
    ## data much better (tho still not great)
    nworked <- sample(1:nsec,N/2,replace=TRUE,prob=dexp(1:nsec,rate=1/lambda))
### this results in a somewhat lower mean than lambda

    studentM <- do.call("c", lapply(seq(N/2),function(n) rep(n,each=nworked[n])))

    ## who works which section? Not sure how to do this right
    ## for now, just make it random. this is _not_ true for CT data
    section <- do.call("c",lapply(seq(N/2),function(n) sample(1:nsec,nworked[n],replace=FALSE)))

    grad <- rbinom(length(section),1,
                  prob=plogis(eta[studentM]+secDiff[section]))


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


system.time(sdat <- do.call("makeRaschDat",parsFromMod))
