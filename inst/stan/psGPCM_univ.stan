data{
//Sample sizes
  int<lower=1> nsecWorked; 
  int<lower=1> nstud; 
  int<lower=1> nsec; 
  int<lower=0> min_k; 
  int<lower=2> max_k; 
  int<lower=0> ncov;
  int<lower=1> nfac; 
  
// prior information
  matrix[nsec, nfac] lambda_prior;

// indices
  int<lower=1,upper=nstud> studentM[nsecWorked];
  int<lower=1,upper=nsec> section[nsecWorked];

// index for factor loadings --------------
  matrix[nsec, nfac] factoridx;
  int<lower=0> firstitem[nsec];
  
// data data
  int<lower=min_k,upper=max_k> grad[nsecWorked];
  matrix[nstud,ncov] X;
  int<lower=0,upper=1> Z[nstud];
  real Y[nstud];
}

parameters{
  vector[nstud] eta;
  real<lower=0> sigU;

  matrix[nsec, nfac] lambda_free;
  vector[max_k-1] tau[nsec];
  
  vector[ncov] betaU;
  vector[ncov] betaY;

  real b00;
  real a1;
  real b0;

  real b1;

  real<lower=0> sigY[2];
}

transformed parameters{

  matrix[nsec, nfac] lambda;

// Factor loading constraints
  for(jjj in 1:nfac) {
    for(jj in 1:nsec) {
	  if(factoridx[jj, jjj] != 0) {
        if(firstitem[jj] == 1) {   // first loading per factor constrained to 1.
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
  matrix[max_k, nsecWorked] p;      // probs of reponse
  matrix[max_k, nsecWorked] s;      // logits of reponse
  
  vector[nstud] muEta;
  vector[nstud] muY0;
  vector[nstud] muY;
  real sigYI[nstud];

  for(i in 1:nstud){
 	  muEta[i] = X[i, ]*betaU;
  
	  muY0[i] = b00 + a1*eta[i] + Z[i] * (b0 + b1*eta[i]);
	  muY[i]  = muY0[i] + X[i,]*betaY;
  
	  sigYI[i] = sigY[Z[i]+1];
  
	  eta[i] ~ normal(muEta[i], sigU);
	  Y[i] ~ normal(muY[i], sigYI[i]);
  };

//priors
  // IRT priors
  for(i in 1:nsec) {
    for(ii in 1:(max_k-1)) {
	    tau[i , ii] ~ uniform(-5, 5);
    };
	  for(j in 1:nfac) {
      lambda_free[i,j] ~ normal(lambda_prior[i,j], 1);
    };
  };

  // PS priors
  //betaY ~ uniform(-5, 5);
  //betaU ~ uniform(-5, 5);
  //a1 ~ uniform(-5, 5);
  //b1 ~ uniform(-5, 5);
  //b00 ~ uniform(-5, 5);
  //b0  ~ uniform(-5, 5);

// Fully Latent Principal Stratification model
  // Latent variable model
  for(i in 1:nsecWorked) {
    s[1,i] = 0; //reference
    for(k in 2:max_k) {
	    s[k,i] = s[k-1, i] + tau[section[i], k-1] + lambda[section[i], 1] * eta[studentM[i]];
	  }
    
    p[,i] = softmax(s[,i]);
	  grad[i] ~ categorical(p[,i]);
  }
}
// last line