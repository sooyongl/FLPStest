library(rstan)

for(i in list.files("R", full.names = T, pattern = "r$")) source(i);

# generate data ------------------------------------------
sim_condition <- list(
  N       = 500, # sample size
  R2Y     = 0.2,
  R2eta   = 0.5,
  ydist   = "n",
  linear  = T,
  lambda  = 1.0,
  nsec    = 20,
  nfac    = 1,
  lvmodel ="rasch"
)

sdat <- do.call("makeDat", sim_condition)
Y <- sdat$stan_dt$Y
Z <- sdat$stan_dt$Z
X <- sdat$stan_dt$X

sdat$stan_dt$section
sdat$stan_dt$studentM
sdat$stan_dt$grad

resp <- matrix(-99, 250, ncol = 20)
for(i in 1:length(sdat$stan_dt$grad)) {
 # i <- 1
 w.item <- sdat$stan_dt$section[i]
 w.peop <- sdat$stan_dt$studentM[i]

 resp[w.peop, w.item]<- sdat$stan_dt$grad[i]

}

resp <- rbind(resp, matrix(-99, 250, ncol = 20))
resp[,1]
dat <- cbind(Y, Z, X, resp)

tail(dat)

write.table(dat, "test/test_mle.csv",
            row.names=FALSE, col.names=FALSE, sep=",")

dat
