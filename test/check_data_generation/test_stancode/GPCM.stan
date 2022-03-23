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

  matrix<lower=0>[nsec, nfac] lambda_free;
  vector[max_k-1] tau[nsec];
  
  vector[ncov] betaU;
}

transformed parameters{

  matrix<lower=0>[nsec, nfac] lambda;

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
  
  for(i in 1:nstud){
 	  muEta[i] = X[i, ]*betaU;
	  eta[i] ~ normal(muEta[i], sigU);
  };

//priors
  // IRT priors
  for(i in 1:nsec) {
    for(ii in 1:(max_k-1)) {
	    tau[i , ii] ~ uniform(-5, 5);
    };
	  for(j in 1:nfac) {
      lambda_free[i,j] ~ normal(lambda_prior[i,j], 1);
	  // lambda_free[i, j] ~ lognormal(0, 0.5)
    };
  };

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