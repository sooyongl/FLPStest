library(lme4)
source('rasch.r')

measMod <- with(sdat,glmer(grad~X[studentM,]+(1|section)+(1|studentM),family=binomial))

## method 1
mod0 <- with(sdat,lm(Y[Z==0]~X[Z==0,]))
mod1 <- with(sdat,lm(Y[Z==1]~X[Z==1,]))

## only two covariates so...
mean((coef(mod1)[-1]-coef(mod0)[-1])/fixef(measMod)[-1])

## pretty close!

### try a simulation

sim1 <- function(){
    sdat <- do.call("makeRaschDat",parsFromMod)

    measMod <- with(sdat,glmer(grad~X[studentM,]+(1|section)+(1|studentM),family=binomial))

## method 1
    mod0 <- with(sdat,lm(Y[Z==0]~X[Z==0,]))
    mod1 <- with(sdat,lm(Y[Z==1]~X[Z==1,]))

## only two covariates so...
    mean((coef(mod1)[-1]-coef(mod0)[-1])/fixef(measMod)[-1])
}
