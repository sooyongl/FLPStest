data{
  //Sample sizes
  int<lower=1> nsecWorked;
  int<lower=1> ncov;
  int<lower=1> nstud;
  int<lower=1> nsec;
  int<lower=1> nfac;
  int<lower=0> min_k;
  int<lower=1> max_k;
  // prior information
  matrix[nsec, nfac] lambda_prior;

  // indices
  int<lower=1,upper=nstud> studentM[nsecWorked];
  int<lower=1,upper=nsec> section[nsecWorked];

  // index for factor loadings
  matrix[nsec, nfac] factoridx;
  int<lower=0> firstitem[nsec];

  // data data
  int<lower=min_k,upper=max_k> grad[nsecWorked];
  matrix[nstud,ncov] X;
  int<lower=0,upper=1> Z[nstud];
  real Y[nstud];
}

parameters{
  // IRT model
  vector[nfac] eta[nstud];       // person scores for each factor
  cholesky_factor_corr[nfac] L; // Cholesky decomp of corr mat of random slopes

  matrix[nsec, nfac] lambda_free; // discrimination of nsec
  real tau[nsec];                 // difficulty of question nsec

  matrix[ncov, nfac] betaU;
  vector[ncov] betaY;

  real b00;
  vector[nfac] a1;
  real b0;

  vector[nfac] b1;

  real<lower=0> sigY[2];
}

transformed parameters {

  matrix[nsec, nfac] lambda;

  // Factor loading constraints
  for(jjj in 1:nfac) {
    for(jj in 1:nsec) {
      if(factoridx[jj, jjj] != 0) {
        if(firstitem[jj] == 1) { // first loading per factor constrained to 1.
        lambda[jj, jjj] = 1;
        } else {
          lambda[jj, jjj] = lambda_free[jj, jjj];
        }
      } else {
        lambda[jj, jjj] = 0;
      }
    }
  };

}

model{
  real linPred[nsecWorked];
  
  vector[nfac] A = rep_vector(1, nfac);
  matrix[nfac, nfac] A0;

  vector[nfac] muEta[nstud];
  vector[nstud] muY0;
  vector[nstud] muY;
  real sigYI[nstud];

  L ~ lkj_corr_cholesky(nfac);
  A0 = diag_pre_multiply(A, L);

  for(i in 1:nstud){

	muEta[i] = to_vector(X[i, ]*betaU);
	
	muY0[i] = b00+ to_row_vector(a1)*eta[i] + Z[i] * (b0 + to_row_vector(b1)*eta[i]);
	muY[i]  = muY0[i] + X[i,]*betaY;
	
	sigYI[i]=sigY[Z[i]+1];
  };

 //priors
    // IRT priors
    //tau ~ normal(0, 1);
	tau ~ uniform(-5, 5);
    for(i in 1:nsec) {
      for(j in 1:nfac) {
        lambda_free[i, j] ~ normal(lambda_prior[i, j], 1);
		//lambda_free[i, j] ~ uniform(-3, 3);
      };
    };

    // PS priors
    betaY ~ uniform(-5, 5);
    for(i in 1:nfac) {
      betaU[,i] ~ uniform(-5, 5);
    };

    a1 ~ uniform(-5, 5);
    b1 ~ uniform(-5, 5);
    b00 ~ uniform(-5, 5);
    b0  ~ uniform(-5, 5);

// Fully Latent Principal Stratification model
    // Latent variable model
	for(j in 1:nsecWorked) {
      linPred[j] = tau[section[j]] + lambda[section[j],1:nfac] * eta[studentM[j]];
	  grad[j] ~ bernoulli_logit(linPred[j]);
	}
	
    // Causal model
	eta ~ multi_normal_cholesky(muEta, A0);
    Y~normal(muY,sigYI);
}
// last line blank
