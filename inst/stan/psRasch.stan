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

 vector[nstud] studEff;

 vector[ncov] betaU;
 vector[ncov] betaY;

 real b00;
 real a1;
 real b0;
 real b1;

 real secEff[nsec];

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
 for(i in 1:nsecWorked)
  linPred[i]= secEff[section[i]]+studEff[studentM[i]];

 for(i in 1:nstud){
  useEff[i]=a1*studEff[i];
  trtEff[i]=b0+b1*studEff[i];
  muY[i]=b00+useEff[i]+Z[i]*trtEff[i];
  sigYI[i]=sigY[Z[i]+1];
 }

 //priors
 betaY~normal(0,2);
 betaU~normal(0,2);
 b00~normal(0,2);
 a1~normal(0,1);
 b0~normal(0,1);
 b1~normal(0,1);

 grad~bernoulli_logit(linPred);

 studEff~normal(X*betaU,sigU);
 Y~normal(muY+X*betaY,sigYI);
}
