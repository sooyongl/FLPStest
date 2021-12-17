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
 real tau[nsec]; // difficulty of question nsec
 real lambda[nsec]; // discrimination of nsec

 // vector[nstud] studEff;
 // real secEff[nsec];

 vector[ncov] betaU;
 vector[ncov] betaY;

 real b00;
 real a1;
 real b0;
 real b1;


 real<lower=0> sigY[2];
 real<lower=0> sigU;
}

model{
 real linPred[nsecWorked];
 vector[nstud] muY;
 real useEff[nstud];
 real trtEff[nstud];
 real sigYI[nstud];


// grad model
 for(i in 1:nsecWorked){

    linPred[i]= lambda[section[i]] * (eta[studentM[i]] - tau[section[i]]);
	}

 for(i in 1:nstud){
  useEff[i]=a1*eta[i];
  trtEff[i]=b0+b1*eta[i];
  muY[i]=b00+useEff[i]+Z[i]*trtEff[i];
  sigYI[i]=sigY[Z[i]+1];
 }

 //priors
 // IRT priors
 tau ~ normal(0, 1);
 lambda ~ normal(1, 1);
 //alpha ~ lognormal(0, .5);

 // PS priors
 betaY~normal(0,2);
 betaU~normal(0,2);
 b00~normal(0,2);
 a1~normal(0,1);
 b0~normal(0,1);
 b1~normal(0,1);

 // Fully Latent Principal Stratification model
 // Latent variable model
 grad~bernoulli_logit(linPred);
 // Causal model
 eta~normal(X*betaU,sigU);
 Y~normal(muY+X*betaY,sigYI);
}
