library(lme4)
source('rasch.r')

measMod <- with(sdat,glmer(grad~X[studentM,]+(1|section)+(1|studentM),family=binomial))

## method 1
mod0 <- with(sdat,lm(Y[Z==0]~X[Z==0,]))
mod1 <- with(sdat,lm(Y[Z==1]~X[Z==1,]))

## only two covariates so...
mean((coef(mod1)[-1]-coef(mod0)[-1])/fixef(measMod)[-1])

source('code/simulations/rasch.r')

#measMod <- with(sdat,glmer(grad~X[studentM,]+(1|section)+(1|studentM),family=binomial))

## method 1
#mod0 <- with(sdat,lm(Y[Z==0]~X[Z==0,]))
#mod1 <- with(sdat,lm(Y[Z==1]~X[Z==1,]))

## only two covariates so...
#mean((coef(mod1)[-1]-coef(mod0)[-1])/fixef(measMod)[-1])

## pretty close!

### try a simulation

sim1 <- function(i){
    sdat <- do.call("makeRaschDat",parsFromMod)

    measMod <- with(sdat,glmer(grad~X[studentM,]+(1|section)+(1|studentM),family=binomial))

## method 1
    mod0 <- with(sdat,lm(Y[Z==0]~X[Z==0,]))
    mod1 <- with(sdat,lm(Y[Z==1]~X[Z==1,]))

## only two covariates so...
    mean((coef(mod1)[-1]-coef(mod0)[-1])/fixef(measMod)[-1])
}

library(parallel)
simOut <- mclapply(1:500,sim1,mc.cores=10)
# simOut <- lapply(1:500,sim1)

save(simOut, file="code/simulations/simpleSim.RData")

system('drive push --no-clobber -quiet')
