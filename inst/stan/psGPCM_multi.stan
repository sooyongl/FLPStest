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
  vector[nfac] eta[nstud];
  cholesky_factor_corr[nfac] L;  

  matrix[nsec, nfac] lambda_free;
  vector[max_k-1] tau[nsec];
  
  matrix[ncov, nfac] betaU;
  vector[ncov] betaY;

  real b00;
  vector[nfac] a1;
  real b0;
 
  vector[nfac] b1;

  real<lower=0> sigY[2];
}

transformed parameters{
  matrix[max_k, nsecWorked] p;      // probs of reponse
  matrix[max_k, nsecWorked] s;      // logits of reponse

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
  
  for(i in 1:nsecWorked) {
    s[1,i] = 0; //reference
    for(k in 2:max_k) {
	  s[k,i] = s[k-1, i] + tau[section[i], k-1] + lambda[section[i], 1:nfac] * eta[studentM[i]];
	}
    p[,i] = softmax(s[,i]);
  }
}

model{
  vector[nfac] A = rep_vector(1, nfac);
  matrix[nfac, nfac] A0;  
  vector[nfac] fac_mean;
 
  vector[nstud] muY;
  real useEff[nstud];
  real trtEff[nstud];
  real sigYI[nstud];

  L ~ lkj_corr_cholesky(nfac);
  A0 = diag_pre_multiply(A, L);
  
  for(i in 1:nstud){
    useEff[i] = to_row_vector(a1)*eta[i];
    trtEff[i] = b0 + to_row_vector(b1)*eta[i];
    muY[i]=b00+useEff[i]+Z[i]*trtEff[i];
    sigYI[i]=sigY[Z[i]+1];
  };

  for(i in 1:nfac) {
    fac_mean[i] = mean(X*betaU[,i]);
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
  for (i in 1:nsecWorked){
    grad[i] ~ categorical(p[,i]);
  }
  // Causal model
  eta ~ multi_normal_cholesky(fac_mean, A0);
  Y~normal(muY+X*betaY,sigYI);
}
// last line