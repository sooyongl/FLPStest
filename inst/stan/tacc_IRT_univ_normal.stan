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
  vector[nstud] eta;
  real<lower=0> sigU;

  matrix<lower=0>[nsec, nfac] lambda_free; // discrimination of nsec
  real tau[nsec];                 // difficulty of question nsec

  vector[ncov] betaU;
  vector[ncov] betaY;

  real b00;
  real a1;
  real b0;

  real b1;

  real<lower=0> sigY[2];
}

transformed parameters {

  matrix<lower=0>[nsec, nfac] lambda;

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

  vector[nstud] muEta;
  vector[nstud] muY0;
  vector[nstud] muY;
  real sigYI[nstud];

  for(i in 1:nstud){

    muEta[i] = X[i, ]*betaU;

	muY0[i] = b00+ a1*eta[i] + Z[i] * (b0 + b1*eta[i]);
	muY[i]  = muY0[i] + X[i,]*betaY;

	sigYI[i]=sigY[Z[i]+1];

	eta[i] ~ normal(muEta[i], sigU);
	Y[i] ~ normal(muY[i], sigYI[i]);
  };

 //priors
    // IRT priors
    tau ~ normal(0, 1);
    for(i in 1:nsec) {
      for(j in 1:nfac) {
        lambda_free[i, j] ~ normal(0, 1);
		// matched prior
	   // lambda_free[i, j] ~ lognormal(0, 5);
        // difuse prior		
		// lambda_free[i, j] ~ uniform(0, 10);
      };
    };

//// PS priors
  betaY ~ uniform(-5, 5);
  betaU ~ uniform(-5, 5);
  a1 ~ uniform(-5, 5);
  b1 ~ uniform(-5, 5);
  b00 ~ uniform(-5, 5);
  b0  ~ uniform(-5, 5);

// Fully Latent Principal Stratification model
    // Latent variable model
	for(j in 1:nsecWorked) {
      linPred[j] = tau[section[j]] + lambda[section[j],1] * eta[studentM[j]];

	  grad[j] ~ bernoulli_logit(linPred[j]);
	}
}
// last line blank
