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

  for(i in 1:nstud){

    muEta[i] = X[i, ]*betaU;

	eta[i] ~ normal(muEta[i], sigU);

  };

 //priors
    // IRT priors
    tau ~ normal(0, 1);
    for(i in 1:nsec) {
      for(j in 1:nfac) {
        //lambda_free[i, j] ~ normal(lambda_prior[i, j], 1);
		//lambda_free[i, j] ~ lognormal(0, 0.5);
		lambda_free[i, j] ~ uniform(0, 5);
		
      };
    };

// Fully Latent Principal Stratification model
    // Latent variable model
	for(j in 1:nsecWorked) {
      linPred[j] = tau[section[j]] + lambda[section[j],1] * eta[studentM[j]];

	  grad[j] ~ bernoulli_logit(linPred[j]);
	}
}
// last line blank
