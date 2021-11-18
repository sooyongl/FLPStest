N = 100

x1 <- rnorm(N)
x2 <- rnorm(N)
Z <- rep(c(1,0),each=N/2)
Y <- rnorm(N)
lv_data <- matrix(1, N*5, ncol = 5, nrow = N)
lv_data <- data.frame(lv_data)


inp_data <- data.frame(Y, Z, x1, x2, lv_data)
outcome = "Y"
group = "Z"
covariate = c("x1", "x2")
lv_model = "F1 =~ X1 + X2 + X3 + X4 + X5"
lv_type = "rasch"

inp_data <- data.frame(inp_data)

outcome.data <- inp_data[outcome]
group.data <- inp_data[group]
covariate.data <- inp_data[covariate]

lv_model1 <- unlist(strsplit(lv_model, "\n"))
lv_model2 <- do.call("rbind",strsplit(lv_model1, "=~"))
lv_model3 <- unlist(strsplit(lv_model2[, 2], "\\+"))
lv_model4 <- unlist(strsplit(lv_model3, " "))

obs.v.name <- lv_model4[lv_model4 != ""]
obs.v.matrix <- inp_data[obs.v.name]

obs.v.partial <- obs.v.matrix[group.data == 1, ]

nsec <- ncol(obs.v.partial)
nstu <- nrow(obs.v.partial)

obs.v.idx <- which(!is.na(obs.v.partial), arr.ind = T)

obs.v.vector <- sapply(1:nrow(obs.v.idx),
                       function(n) obs.v.matrix[obs.v.idx[n,1], obs.v.idx[n,2]])


flps_data <- list(
  nsecWorked = length(obs.v.idx[,2]),
  nstud = nstu,
  nsec = nsec,

  studentM = obs.v.idx[,1],
  section = obs.v.idx[,2],

  grad = obs.v.vector,
  X = covariate.data,
  ncov = ncol(covariate.data),

  Z = c(group.data),
  Y = c(outcome.data)
)

out <- new("flpsData")

out@outcome <- outcome
out@group <- group
out@lv_type <- lv_type
out@lv_model <- lv_model
out@flps_data <- flps_data


out


# make inp data

makeInpData <- function(N,R2Y,omega,tau0,tau1,lambda,R2eta,nsec,lvmodel){

  lvmodel <- tolower(lvmodel)

  ## generate covariates ----------------------------------------------------

  ### Auxiliary Covariates
  x1 <- rnorm(N); #x1 <- x1 - mean(x1); x1 <- x1/sd(x1)
  x2 <- rnorm(N); #x2 <- x2 - mean(x2); x2 <- x2/sd(x2) # in proposal, it is binary.
  ### Treatment or Control
  Z <- rep(c(1,0),each=N/2)

  # Generate True eta -------------------------------------------------------
  ## generate etas
  random.e <- rnorm(N,0,sqrt(1-R2eta))
  eta <- sqrt(R2eta/2)*(x1-x2) + random.e

  # random.e <- rnorm(N,0,sqrt(1))
  # eta <- 0*(x1-x2) + random.e

  # Generate LVM data -------------------------------------------------------
  info <- parsForLVM(theta = eta, nsec = nsec, data_type = lvmodel)
  grad <- generate(info); # methods(generate)

  lv.par <- grad$lv.par
  grad <- grad$resp

  nworked <- sample(1:nsec,N/2,replace=TRUE,prob=dexp(1:nsec,rate=1/lambda))
  studentM <- do.call("c", lapply(seq(N/2),function(n) rep(n,each=nworked[n])))

  section <- do.call("c", lapply(seq(N / 2),
                                 function(n) {
                                   sort(sample(1:nsec, nworked[n],
                                               replace = FALSE))}))
  ss <- cbind(studentM, section)

  d1 <- matrix(rep(NA, nsec*(N/2)), ncol = nsec)
  s1 <- split(ss[,2], studentM);
  for(i in 1:length(s1)) { d1[i, s1[[i]]] <- grad[i, s1[[i]]]  }

  d2 <- matrix(rep(NA, nsec*(N/2)), ncol = nsec)
  d3 <- rbind(d1, d2)
  # grad <- sapply(1:dim(ss)[1], function(n) grad[ss[n,1], ss[n,2]] )

  ### simulate Y -------------------------------------------------------------
  random.Y <- rnorm(N,0,sqrt(1-R2Y))

  # Y <- sqrt(R2Y/2)*(x2-x1)+random.Y
  # Y <- Y + omega*eta
  # Y <- Y + Z*(tau0 + tau1*eta)

  Y <- tau0*Z + omega*eta + tau1*eta*Z + sqrt(R2Y/2)*(x2-x1) + random.Y
  # Y <- tau0*Z + omega*eta + tau1*eta*Z + 0*(x2-x1) + random.Y

  inp_data <- data.frame(Y, Z, cbind(x1, x2), d3)

  list(
    lv.par = lv.par,
    true_eta = eta,
    inp_data = inp_data
  )
}




















