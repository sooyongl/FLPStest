data{
//Sample sizes
 int<lower=1> nsecWorked;
 int<lower=1> ncov;
 int<lower=1> nstud;
 int<lower=1> nsec;

// indices
 int<lower=1,upper=nstud> studentM[nsecWorked];
 int<lower=1,upper=nsec> section[nsecWorked];

// data data
 int<lower=0,upper=1> grad[nsecWorked];
 matrix[nstud,ncov] X;
 int<lower=0,upper=1> Z[nstud];
 real Y[nstud];

}

parameters{
 // IRT model
 // real delta; // mean ability
 vector[nstud] eta; // ability of student nstud
 real lambda_free[nsec]; // discrimination of nsec
 real tau_free[nsec]; // difficulty of question nsec
 
 // vector[nstud] studEff;
 // real secEff[nsec];

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

transformed parameters {
  vector[nsec] lambda;
  vector[nsec] tau;

  real linPred[nsecWorked];

  for(jj in 1:nsec) {
    if(jj == 1) {
      lambda[1] = 1;
	  tau[1] = 0;
	} else {
      lambda[jj] = lambda_free[jj];
	  tau[jj] = tau_free[jj];
    }  
  }
 
  for(j in 1:nsecWorked) {
    linPred[j] = tau[section[j]] + lambda[section[j]] * eta[studentM[j]];
  }

}

model{
 vector[nstud] muY;
 real useEff[nstud];
 real trtEff[nstud];
 real sigYI[nstud];

 for(i in 1:nstud){
  useEff[i]=a1*eta[i];
  trtEff[i]=b0+b1*eta[i];
  muY[i]=b00+useEff[i]+Z[i]*trtEff[i];
  sigYI[i]=sigY[Z[i]+1];
 }

 //priors
 // IRT priors
 tau_free ~ normal(0,1);// uniform(-10, 10);
 lambda_free ~ normal(0,1);

 // PS priors
 betaY~normal(0,1);

 betaU[1]~normal(0,1);
 betaU[2]~normal(0,1);
 
 a1~normal(0,1);
 b1~normal(0,1);
 
 b00~normal(0,1);
 b0~normal(0,1);
 
 // Fully Latent Principal Stratification model
 // Latent variable model
 grad~bernoulli_logit(linPred);
 // Causal model
 //eta~normal(muEta+X*betaU,sigU);
 eta~normal(X*betaU,sigU);
 Y~normal(muY+X*betaY,sigYI);
}
// last line blank