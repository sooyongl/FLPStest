#' Generate Fully Latent Principal Stratification data
#'
#' @description
#' \code{\link{makeDat}} is a function for generating a data based on the given
#' information.
#' @param N a numeric indicating sample size.
#' @param R2Y a numeric indicating predictive power of covariates.
#' @param omega a numeric indicating the relationship between eta_T and Y_C.
#' @param tau0 a numeric indicating the treatment eff when eta_T=0.
#' @param tau1 a numeric indicating the relationship between eta_T and Y_T-Y_C;
#' (for now, single level (not multilevel), linear, normal).
#' @param lambda a numeric indicating the mean of Worked problems/person.
#' @param R2eta a numeric indicating Predictive power of latent variable
#'  (extent to which covariates predict eta).
#' @param nsec a numeric indicating the number of maximum sections given to
#'  students.
#' @param nfac a numeric indicating the number of latent factors
#' @param lvmodel a character specifying a type of latent variable model.
#' @param lvinfo a list containing the information corresponding the latent model.
#' @return a list containing all the data related to population values and running FLPS.
#'
#' @examples
#' sdat <- makeDat(
#'   N = 100,
#'   R2Y = 0.2,
#'   omega = 0.2,
#'   tau0 = 0.4,
#'   tau1 = -0.2,
#'   lambda = .6,
#'   R2eta = 0.5,
#'   linear = T,
#'   nsec = 10,
#'   lvmodel = "2PL"
#' )
#'
#' @export
makeDat <- function(N,R2Y,R2eta,linear,ydist,lambda,nsec,nfac,lvmodel){

  mc <- match.call(expand.dots = TRUE)
  mc[[1L]] <- quote(list); # mc <- as.list(match.call()[-1])

  # set up S3 class ---------------------------------------------------------
  sim_info <- structure(eval(mc), class = tolower(lvmodel))
  # sim_info <- structure(sim_condition, class = tolower(sim_condition$lvmodel))

  # Generate Latent Variable Model Information ------------------------------
  sim_info <- genLVinfo(sim_info)

  # Generate True eta -------------------------------------------------------
  sim_info <- genTrueEta(Data=sim_info)

  # Generate LV part --------------------------------------------------------
  sim_info <- genLVM(sim_info)

  # simulate Y --------------------------------------------------------------
  sim_info <- genOutcome(sim_info)

  return(sim_info)
}

#' Generate a matrix style data for simulation
#'
#' @description
#' \code{\link{makeInpDat}} is a function for generating a data based on
#' the given information.
#' @param N a numeric indicating sample size.
#' @param R2Y a numeric indicating predictive power of covariates.
#' @param omega a numeric indicating the relationship between eta_T and Y_C.
#' @param tau0 a numeric indicating the treatment eff when eta_T=0.
#' @param tau1 a numeric indicating the relationship between eta_T and Y_T-Y_C;
#' (for now, single level (not multilevel), linear, normal).
#' @param lambda a numeric indicating the mean of Worked problems/person.
#' @param R2eta a numeric indicating Predictive power of latent variable
#' (extent to which covariates predict eta).
#' @param nsec a numeric indicating the number of maximum sections given to students.
#' @param lvmodel a character specifying a type of latent variable model.
#' @return a list containing latent variable model information, true eta,
#' and a matrix containing the data for running FLPS.
#'
#' @examples
#' sdat <- makeInpData(
#'   N = 100,
#'   R2Y = 0.2,
#'   omega = 0.2,
#'   tau0 = 0.13,
#'   tau1 = -0.06,
#'   lambda = 10,
#'   R2eta = 0.5,
#'   nsec = 10,
#'   linear = T,
#'   lvmodel = "sem",
#'   lvinfo  = 1
#' )
#' @export
makeInpData <- function(N,R2Y,omega,tau0,tau1,lambda,R2eta,linear,nsec,lvmodel, lvinfo){

  mc <- match.call(expand.dots = TRUE)
  mc[[1L]] <- quote(list); # mc.list <- as.list(match.call(mc)[-1]))}

  # set up S3 class ---------------------------------------------------------
  sim_info <- structure(eval(mc), class = tolower(lvmodel))
  # sim_info <- structure(setup_dat$sim_condition, class = tolower(setup_dat$sim_condition$lvmodel))

  # Generate True eta -------------------------------------------------------
  sim_info <- genTrueEta(sim_info)

  # Generate LV part --------------------------------------------------------
  sim_info <- genLVM(sim_info)

  d1 <- matrix(rep(NA, nsec*(N/2)), ncol = nsec)
  s1 <- split(cbind(sim_info$studentM, sim_info$section)[,2], sim_info$studentM);
  for(i in 1:length(s1)) {d1[i, s1[[i]]] <- sim_info$lv.resp[i, s1[[i]]]}

  d2 <- matrix(rep(NA, nsec*(N/2)), ncol = nsec)
  d3 <- rbind(d1, d2)

  # simulate Y --------------------------------------------------------------
  sim_info <- genOutcome(sim_info)
  inp_data <- data.frame(sim_info$stan_dt$Y, sim_info$stan_dt$Z, sim_info$x, d3)

  list(
    sim_info = sim_info,
    inp_data = inp_data
  )

  # lvmodel <- tolower(lvmodel)
  #
  # ## generate covariates ----------------------------------------------------
  #
  # # Generate True eta -------------------------------------------------------
  # ## generate eta
  # ## generate eta
  # if(lvmodel == "lgm") {
  #   data <- genTrueEta.lgm(N, R2eta, linear, lvinfo$nfac, lvinfo$growth_mean)
  # } else {
  #   data <- genTrueEta(N, R2eta, linear)
  # }
  #
  # # Generate LVM data -------------------------------------------------------
  # info <- parsForLVM(theta = data[, grep("eta", colnames(data))],
  #                    nsec = nsec, data_type = lvmodel, lv_info = lvinfo)
  # info.data <- generate(info); # methods(generate)
  #
  # lv.par <- info.data$lv.par
  # grad <- info.data$resp
  #
  # # nworked <- sample(1:nsec,N/2,replace=TRUE,prob=dexp(1:nsec,rate=1/lambda))
  # nworked <- rep(floor(nsec * lambda), N/2)
  #
  # studentM <- do.call("c", lapply(seq(N/2),function(n) rep(n,each=nworked[n])))
  #
  # section <- do.call("c", lapply(seq(N/2),
  #                                function(n) {
  #                                  sort(sample(1:nsec, nworked[n],
  #                                              replace = FALSE))}))
  # ss <- cbind(studentM, section)
  #
  # d1 <- matrix(rep(NA, nsec*(N/2)), ncol = nsec)
  # s1 <- split(ss[,2], studentM);
  # for(i in 1:length(s1)) {d1[i, s1[[i]]] <- grad[i, s1[[i]]]}
  #
  # d2 <- matrix(rep(NA, nsec*(N/2)), ncol = nsec)
  # d3 <- rbind(d1, d2)
  #
  # ### simulate Y -------------------------------------------------------------
  # ### Treatment or Control
  # Z <- rep(c(1,0), each=N/2)
  #
  # if(lvmodel %in% c("lca", "lpa")) {
  #   Y.res = 1 - R2Y
  #   eta <- data[, "eta"]
  #   x1 <- data[,"x1"]
  #   x2 <- data[,"x2"]
  #
  #   Y <- rep(0, N)
  #
  #   idx.c1 <- which(info.data$resp[, "class"]==0)
  #   Y[idx.c1] <- 0 + 0*Z[idx.c1]
  #
  #   idx.c2 <- which(info.data$resp[, "class"]==1)
  #   Y[idx.c2] <- omega + tau1*Z[idx.c2]
  #
  #   if(linear) {
  #     Y <- tau0*Z + 1*x1 + 0.5*x2 + rnorm(N, 0, Y.res)
  #   } else {
  #     x1sq <- data[,"x1sq"]
  #     Y <- tau0*Z + 1*x1 + 0.5*x1sq + 0.5*x2 + rnorm(N, 0, Y.res)
  #   }
  #
  # } else {
  #
  #   Y <- genOutcome(cbind(Z,data), R2Y, omega, tau0, tau1, linear)
  # }
  #
  # inp_data <- data.frame(Y, Z, data[, grepl("x", colnames(data))], d3)
  #
  # list(
  #   lv.rep = info.data$resp,
  #   lv.par = lv.par,
  #   true_eta = data[, grep("eta", colnames(data))],
  #   inp_data = inp_data
  # )
}
