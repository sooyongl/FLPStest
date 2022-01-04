rm(list = ls())
for(i in fs::dir_ls("R", regexp = "r$")[-1]) source(i); rm(i)
source_funs <- ls()


conditions <- data.frame(
  samplesize = c(1000, 4000, 1000, 4000),
  lambda = c(10,10, 40, 40),
  nsec = c(20, 20, 100, 100)
)

conditions <- crossing(conditions, rep = 1:500)

prtSeq <- round(seq(1,nrow(conditions),length=11))
results <- sapply(1:nrow(conditions),function(j){
  if(j%in%prtSeq) cat(round(j/nrow(conditions)*100),'% ',sep='')
  
  REP      <- conditions$rep[j]
  
  N      <- conditions$samplesize[j]
  lambda <- conditions$lambda[j]
  nsec   <- conditions$nsec[j]
  
  # data generation ---------------------------------------------------------
  parsFromMod <- list(
    N = N, # sample size
    R2Y = 0.2, ## from app
    omega = 0.2,
    tau0 = 0.13, ## from paper
    tau1 = -0.06, ## from paper "a difference of one IQR in etaT was associated with a reduction of 0.083 in the effect size" 0.083/1.35~~ 0.06
    lambda = lambda, ## from data used in model
    R2eta = 0.5, ## from app
    nsec = nsec, ## from data used in model
    lvmodel = "rasch" # tag for latent variable model
  )
  
  
  # set.seed(i+N+nsec)
  
  sdat <- do.call("makeDat", parsFromMod)
  
  interactionMethod(sdat=sdat,mlMeas = FALSE)
  
}
)

save(cbind(conditions,t(results)),
     file=paste0('interactionMethodResults',Sys.Date(),'.RData'))

