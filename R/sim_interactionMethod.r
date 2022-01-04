#' obtain population value
#'
interactionMethod <- function(sdat,mlMeas=TRUE){
  measMod <- if(mlMeas){
    with(sdat,glmer(grad~X[studentM,]+(1|section)+(1|studentM),family=binomial))
  } else with(sdat,glm(grad~X[studentM,]+as.factor(section),family=binomial))

  mod0 <- with(sdat,lm(Y[Z==0]~X[Z==0,]))
  mod1 <- with(sdat,lm(Y[Z==1]~X[Z==1,]))

  b0 <- coef(mod1)[1]-coef(mod0)[1]

  coefDiff <- coef(mod1)[-1]-coef(mod0)[-1]
  lvCoef <- if(mlMeas){
    fixef(measMod)[-1]
  } else coef(measMod)[2:(ncol(sdat$X)+1)]

  c(b0=b0,b1=coef(lm(coefDiff~lvCoef))['lvCoef'])
}



