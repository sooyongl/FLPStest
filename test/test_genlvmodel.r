# https://rpubs.com/okanbulut/pcmsimulation

# GPCM ---------------------------------------------------------------------
set.seed(668)
thresholds <- t(apply(matrix(runif(10*3, .3, 1), 10), 1, cumsum))
thresholds <- -(thresholds - rowMeans(thresholds))
thresholds <- thresholds + rnorm(10)
thresholds <- thresholds*-1
thresholds
a <- rlnorm(10, 0.1, 0.3)


steps
theta <- rnorm(10000, mean=0, sd=1)
simData.pcm(info)




responses <- mirt::simdata(
  a = as.matrix(a),
  d = as.matrix(thresholds),
  guess = 0,
  N = length(theta),
  Theta = as.matrix(theta),
  itemtype = "gpcm")
cbind(a,thresholds)

library(skimr)
library(mirt)
skim(responses)
responses <- data.frame(responses)

model.pcm <- 'f = 1-10'
results.pcm <- mirt(data=responses, model=model.pcm, itemtype="gpcm", SE=TRUE, verbose=FALSE)
coef.pcm <- coef(results.pcm, IRTpars=TRUE, simplify=TRUE)
items.pcm <- as.data.frame(coef.pcm$items)
items.pcm

coef.pcm <- coef(results.pcm, IRTpars=F, simplify=TRUE)
items.pcm <- as.data.frame(coef.pcm$items)
items.pcm

cbind(a,steps[,2:4]+steps[,1])
cbind(a,thresholds)
# Save responses as a data frame and rename the columns
responses <- as.data.frame(responses)
colnames(responses) <- paste0("item_", 1:10)
head(responses)

# -------------------------------------------------------------------------
ipar <- genIRTpar(20, ncat = 3,nfac = 1, "gpcm")
info <- list(
  theta = matrix(rnorm(2000), ncol = 1),
  nsec = 20,
  lvinfo = list(ipar = ipar, nfac = 1),
  lvmodel = "gpcm"
)
gdata <- generateLV.irt(info)
max(gdata$resp)
min(gdata$resp)
model.gpcm <- 'F = 1-20'

results.gpcm <- mirt::mirt(data=gdata$resp, model=model.gpcm, itemtype="gpcm", SE=TRUE, verbose=FALSE)
coef.gpcm <- mirt::coef(results.gpcm, IRTpars=TRUE, simplify=T)$item
coef.gpcm <- mirt::coef(results.gpcm, IRTpars=F, simplify=T)$item
gdata$lv.par
