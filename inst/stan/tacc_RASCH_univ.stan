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

transformed data {
   matrix[nsec, nfac] lambda = factoridx;

}

parameters{
  // IRT model
  vector[nstud] eta;
  real<lower=0> sigU;

  real tau[nsec];                 // difficulty of question nsec

  //matrix[ncov, nfac] betaU;
  vector[ncov] betaU;
  vector[ncov] betaY;

  real b00;
  //vector[nfac] a1;
  real a1;
  real b0;

  //vector[nfac] b1;
  real b1;

  real<lower=0> sigY[2];
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

      linPred[j] = tau[section[j]] + eta[studentM[j]];

	  grad[j] ~ bernoulli_logit(linPred[j]);
	}
}
