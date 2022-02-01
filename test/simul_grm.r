simdata
function (a,
          d,
          N,
          itemtype,
          sigma = NULL,
          mu = NULL,
          guess = 0,
          upper = 1,
          nominal = NULL,
          t = NULL,
          Theta = NULL,
          gpcm_mats = list(),
          returnList = FALSE,
          model = NULL,
          equal.K = TRUE,
          which.items = NULL,
          mins = 0,
          lca_cats = NULL,
          prob.list = NULL)

# {
  # if (!is.null(prob.list)) {
  #   if (!all(sapply(prob.list, function(x) is.matrix(x) || 
  #                   is.data.frame(x)))) 
  #     stop("Elements of prob.list must be either a matrix or data.frame")
  #   prob.list <- lapply(prob.list, as.matrix)
  #   if (!all(sapply(prob.list, nrow) == nrow(prob.list[[1L]]))) 
  #     stop("prob.list elements have unequal rows")
  #   K <- sapply(prob.list, ncol)
  #   nitems <- length(K)
  #   if (any(K == 1L)) 
  #     stop("prob.list elements should have more than 1 column")
  #   if (length(mins) == 1L) 
  #     mins <- rep(mins, nitems)
  #   stopifnot(length(mins) == nitems)
  #   data <- matrix(NA, nrow(prob.list[[1L]]), nitems)
  #   for (i in 1L:nitems) data[, i] <- respSample(prob.list[[i]])
  #   data <- (t(t(data) + mins))
  #   colnames(data) <- paste("Item_", 1L:nitems, sep = "")
  #   return(data)
  # }
  # if (!is.null(model)) {
  #   stopifnot(is(model, "SingleGroupClass"))
  #   nitems <- extract.mirt(model, "nitems")
  #   if (is.null(which.items)) 
  #     which.items <- 1L:nitems
  #   nfact <- extract.mirt(model, "nfact")
  #   cfs <- coef(model, simplify = TRUE)
  #   if (is.null(sigma)) 
  #     sigma <- cfs$cov
  #   if (is.null(mu)) 
  #     mu <- cfs$means
  #   if (is.null(Theta)) {
  #     if (missing(N)) 
  #       N <- nrow(extract.mirt(model, "data"))
  #     Theta <- mirt_rmvnorm(N, mu, sigma, check = TRUE)
  #   }
  #   else N <- nrow(Theta)
  #   data <- matrix(0, N, nitems)
  #   colnames(data) <- extract.mirt(model, "itemnames")
  #   K <- extract.mirt(model, "K")
  #   for (i in which.items) {
  #     obj <- extract.item(model, i)
  #     P <- ProbTrace(obj, Theta)
  #     data[, i] <- respSample(P)
  #     if (equal.K) 
  #       while (length(unique(data[, i])) != K[i]) data[, 
  #                                                      i] <- respSample(P)
  #   }
  #   ret <- t(t(data) + model@Data$mins)
  #   return(ret[, which.items, drop = FALSE])
  # }
  
  
  if (missing(d)) 
    d <- matrix(1, nrow(a))
  if (is.vector(d)) 
    d <- matrix(d)
  
  nfact <- ncol(a)
  nitems <- nrow(a)
  
  if (length(mins) == 1L) {
    mins <- rep(mins, nitems)
  }
  
  K <- rep(0L, nitems)
  
  if (length(guess) == 1L) {
    guess <- rep(guess, nitems)
  }
  
  if (length(upper) == 1L) {
    upper <- rep(upper, nitems)
  }
  
  
  if (length(itemtype) == 1L) {
    itemtype <- rep(itemtype, nitems)
  }
  
  use_gpcm_mats <- rep(FALSE, nitems)
  
  if (any(itemtype %in% mirt:::Valid_iteminputs())) {
    itemtype <- mirt:::toInternalItemtype(itemtype)
  }
  
  for (i in 1L:length(K)) {
    K[i] <- length(na.omit(d[i, ])) + 1L
    
    if (any(itemtype[i] == c("gpcm", "nominal", "nestlogit"))) {
      K[i] <- K[i] - 1L
    }
  }
  
  K <- as.integer(K)
  
  guess <- mirt:::logit(guess)
  upper <- mirt:::logit(upper)
  
  oldguess <- guess
  oldupper <- upper
  
  guess[K > 2L] <- upper[K > 2L] <- NA
  
  sigma <- diag(nfact)
  
  mu <- rep(0, nfact)
  
  Theta <- mirt:::mirt_rmvnorm(N, mu, sigma, check = TRUE)
  
  if(is.null(nominal)){ 
    nominal <- matrix(NA, nitems, max(K))
  }
  
  data <- matrix(0, N, nitems)
  a[is.na(a)] <- 0
  
  itemobjects <- vector("list", nitems)
  
  for (i in 1L:nitems) { # i = 1
    # if (itemtype[i] == "nestlogit") {
    #   par <- na.omit(c(a[i, ], d[i, 1], guess[i], upper[i], 
    #                    nominal[i, -1L], d[i, -1L]))
    #   obj <- new(itemtype[i], par = par, nfact = nfact, 
    #              correctcat = 1L)
    # } else {
    # if (itemtype[i] == "gpcm") {
    #   if (!use_gpcm_mats[i]) {
    #     par <- na.omit(c(a[i, ], 0:(K[i] - 1), d[i, 
    #     ]))
    #   } else {
    #     stopifnot(nrow(gpcm_mats[[i]]) == K[i])
    #     stopifnot(ncol(gpcm_mats[[i]]) == nfact)
    #     par <- na.omit(c(a[i, ], as.vector(gpcm_mats[[i]]), 
    #                      d[i, ]))
    #   }
    # } else if (itemtype[i] == "ideal") {
    #   if (K[i] > 2) 
    #     stop("ideal point models for dichotomous items only", 
    #          call. = FALSE)
    #   if (d[i, 1] > 0) 
    #     stop("ideal point intercepts must be negative", 
    #          call. = FALSE)
    #   par <- na.omit(c(a[i, ], d[i, ]))
    # } else if (itemtype[i] == "lca") {
    #   par <- na.omit(a[i, ])
    # }
    # else {
      # if (itemtype[i] == "nominal") {
      #   if (length(na.omit(nominal[i, ])) != length(na.omit(d[i, 
      #   ]))) 
      #     stop("nominal and d inputs must have same length for nominal reponse model", 
      #          call. = FALSE)
      # }
      par <- na.omit(c(a[i, ], nominal[i, ], d[i, ], guess[i], upper[i]))
    # }
      
    obj <- new(itemtype[i], par = par, nfact = nfact, ncat = K[i])
    
    
    # if (itemtype[i] %in% c("gpcm", "nominal")) 
    #   obj@mat <- FALSE
    # if (use_gpcm_mats[i]) 
    #   obj@mat <- TRUE
    # }
    # if (itemtype[i] == "ggum") {
    #   if (length(na.omit(a[i, ])) != length(na.omit(d[i, 
    #   ]))) 
    #     stop("ggums must have the same number of a and d values per item", 
    #          call. = FALSE)
    #   par <- c(na.omit(a[i, ]), -d[i, ], t[i, ])
    #   obj <- new(itemtype[i], par = par, nfact = nfact, 
    #              ncat = K[i])
    # }
    # if (any(itemtype[i] == c("gpcm", "nominal", 
    #                          "nestlogit", "ggum"))) 
    #   obj@ncat <- K[i]
    
    ################################################
    # this is the core function!!!!!!!!!!!!!!!!!!!!!
    ################################################
    P <- mirt:::ProbTrace(obj, Theta); # ProbTrace,graded,matrix-method
    
    data[, i] <- mirt:::respSample(P)
    
    itemobjects[[i]] <- obj
  # }
  
  data <- (t(t(data) + mins))
  colnames(data) <- paste("Item_", 1L:nitems, sep = "")
  if (returnList) {
    return(list(itemobjects = itemobjects, data = data, Theta = Theta))
  }
  else {
    return(data)
  }
  # }


# -------------------------------------------------------------------------

  P.poly <- function(par, Theta, itemexp = FALSE, ot = 0)
  {
    return(.Call('gradedTraceLinePts', par, Theta, itemexp, ot, FALSE))
  }
  
  setMethod(
    f = "ProbTrace",
    signature = signature(x = 'graded', Theta = 'matrix'),
    definition = 
      function(x, Theta, itemexp = TRUE, useDesign = TRUE, ot=0){
        
        x = obj
        Theta = Theta
      #   
      # if(nrow(x@fixed.design) > 1L && useDesign)
      #   Theta <- cbind(x@fixed.design, Theta)
      # return(
        
        mirt:::P.poly(x@par, Theta=Theta, itemexp=itemexp, ot=ot)
        
        # )
    }
  )
  
  
  gradedTraceLinePts
  // graded
  RcppExport SEXP gradedTraceLinePts(SEXP Rpar, SEXP RTheta, SEXP Ritemexp, SEXP Rot, SEXP Risrating)
  {
    BEGIN_RCPP
    
    const vector<double> par = as< vector<double> >(Rpar);
    const NumericVector ot(Rot);
    const NumericMatrix Theta(RTheta);
    const int nfact = Theta.ncol();
    const int N = Theta.nrow();
    const int itemexp = as<int>(Ritemexp);
    const int israting = as<int>(Risrating);
    int nint = par.size() - nfact;
    if(israting) --nint;
    int totalcat = nint + 1;
    if(!itemexp) ++totalcat;
    vector<double> P(N * totalcat);
    P_graded(P, par, Theta, ot, N, nfact, nint, itemexp, israting);
    NumericMatrix ret = vec2mat(P, N, totalcat);
    return(ret);
    
    END_RCPP
  }
  
  
  void P_graded(vector<double> &P, const vector<double> &par,
                const NumericMatrix &Theta, const NumericVector &ot, const int &N,
                const int &nfact, const int &nint, const int &itemexp, const int &israting)
  {
    const int parsize = par.size();
    vector<double> a(nfact);
    for(int i = 0; i < nfact; ++i) a[i] = par[i];
    vector<double> d(nint, 0.0);
    if(israting){
      const double t = par[parsize-1];
      for(int i = nfact; i < parsize - 1; ++i)
        d[i - nfact] = par[i] + t;
    } else {
      for(int i = nfact; i < parsize; ++i)
        d[i - nfact] = par[i];
    }
    int notordered = 0;
    for(int i = 1; i < nint; ++i)
      notordered += d[i-1] <= d[i];
    if(notordered){
      int P_size = P.size();
      for(int i = 0; i < P_size; ++i)
        P[i] = 0.0;
    } else {
      const double nullzero = 0.0, nullone = 1.0;
      NumericMatrix Pk(N, nint + 2);
      
      for(int i = 0; i < N; ++i)
        Pk(i,0) = 1.0;
      for(int i = 0; i < nint; ++i){
        vector<double> tmp1(N), tmp2(N);
        itemTrace(tmp1, tmp2, a, &d[i], Theta, &nullzero, &nullone, ot);
        for(int j = 0; j < N; ++j)
          Pk(j,i+1) = tmp2[j];
      }
      if(itemexp){
        int which = N * (nint + 1) - 1;
        for(int i = (Pk.ncol()-2); i >= 0; --i){
          for(int j = (N-1); j >= 0; --j){
            P[which] = Pk(j,i) - Pk(j,i+1);
            if(P[which] < 1e-50) P[which] = 1e-50;
            else if((1.0 - P[which]) < 1e-50) P[which] = 1.0 - 1e-50;
            --which;
          }
        }
      } else {
        int which = 0;
        for(int i = 0; i < Pk.ncol(); ++i){
          for(int j = 0; j < Pk.nrow(); ++j){
            P[which] = Pk(j,i);
            ++which;
          }
        }
      }
    }
  }
  
  
  