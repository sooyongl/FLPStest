parsFromMod <- list(
  N = 500, # sample size
  R2Y = 0.2,
  omega = 0.2,  # a1
  tau0 = 0.5,  # b0
  tau1 = -0.3, # b1
  # lambda = 10,
  lambda = 0.6,
  R2eta = 0.2,
  nsec = 20,
  lvmodel = "sem" # tag for latent variable model
)

sdat <- do.call("makeInpData", parsFromMod)

df <- sdat$inp_data[1:250,paste0("X",1:20)]

apply(df, 2, function(x) {
  cor(x, rowMeans(df, na.rm = T), use = "pairwise.complete.obs")
})


