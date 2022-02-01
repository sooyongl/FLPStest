data{
//Sample sizes
  int<lower=1> nsecWorked;                        // number of rows in long-format data
  int<lower=1> nstud;                             // number of students (I)
  int<lower=1> nsec;                              // number of items    (J)
  int<lower=2> max_k;                             // Max category
  int<lower=0> ncov;
  
// prior information
 real lambda_prior[nsec];
 
  // indices
  int<lower=1,upper=nstud> studentM[nsecWorked];
  int<lower=1,upper=nsec> section[nsecWorked];

// data data
  int<lower=1,upper=max_k> grad[nsecWorked];
  matrix[nstud,ncov] X;
  int<lower=0,upper=1> Z[nstud];
  real Y[nstud];

}

parameters{
  // IRT model
  // real delta; // mean ability
  vector[nstud] eta; // ability of student nstud

  real lambda_free[nsec];                  // discrimination of nsec
  //real tau_free[nsec, max_k-1];          // thresholds parameters
  ordered[max_k-1] tau_free[nsec]; //item category intercept

  vector[ncov] betaU;
  vector[ncov] betaY;

  //real muEta;
  real b00;
  real a1;
  real b0;
  real b1;

  real<lower=0> sigY[2];
  real<lower=0> sigU;
}

transformed parameters{
   
  real lambda[nsec];       // new disc with the first loading fixed to 1
  //real tau[nsec, max_k-1]; // new threshold with the first cate of first item fixed to 0
  ordered[max_k-1] tau[nsec];
  
  for(i in 1:nsecWorked){
	
    if(section[i] == 1){
	  lambda[section[i]] = 1;
	  tau[section[i],1] = 0;
	  for(kk in 2:(max_k-1)) {
	    tau[section[i],kk] = tau_free[section[i],kk];
	  }
	} else {
	  lambda[section[i]] = lambda_free[section[i]];
	  for(kk in 1:(max_k-1)) {
	    tau[section[i],kk] = tau_free[section[i],kk];
	  }
	}
  }
}

model{
  //real linPred[nsecWorked];
  vector[nstud] muY;
  real useEff[nstud];
  real trtEff[nstud];
  real sigYI[nstud];

  for (i in 1:nsecWorked){
    grad[i] ~ ordered_logistic(eta[studentM[i]]*lambda[section[i]],tau[section[i]]);
  }

  for(i in 1:nstud){
    useEff[i]=a1*eta[i];
    trtEff[i]=b0+b1*eta[i];
    muY[i]=b00+useEff[i]+Z[i]*trtEff[i];
    sigYI[i]=sigY[Z[i]+1];
  }

  //priors
  // IRT priors
  for(i in 1:nsec) {
   lambda_free[i] ~ normal(lambda_prior[i], 1);
    for(ii in 1:(max_k-1)) {
      tau_free[i , ii] ~ uniform(-5, 5);
   }
  };
  

  // PS priors
  //muEta~normal(0, sqrt(1));
  betaY~normal(0,2);
  betaU~normal(0,2);
  b00~normal(0,2);
  a1~normal(0,1);
  b0~normal(0,1);
  b1~normal(0,1);

  // Fully Latent Principal Stratification model

  eta~normal(X*betaU,sigU);
  Y~normal(muY+X*betaY,sigYI);
}
