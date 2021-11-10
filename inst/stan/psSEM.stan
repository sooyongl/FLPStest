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
 real grad[nsecWorked];
 matrix[nstud,ncov] X;
 int<lower=0,upper=1> Z[nstud];
 real Y[nstud];

}

parameters{

 vector[nstud] eta;

 vector[ncov] betaU;
 vector[ncov] betaY;

 real b00;
 real a1;
 real b0;
 real b1;

 real lambda[nsec];

 real<lower=0> sigR;
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
 for(i in 1:nsecWorked) {

  linPred[i] = lambda[section[i]] * eta[studentM[i]];
}

 for(i in 1:nstud){
  useEff[i]=a1*eta[i];
  trtEff[i]=b0+b1*eta[i];
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

 eta ~ normal(0, sqrt(1));
 lambda ~ normal(0, sqrt(1));
 sigR ~ inv_gamma(2.1, 1.1);
 // error variance prior
 // for(i in 1:Nv){
 //   // sigma2 prior
 //   if(dsigma2[i] == 0){
 //    sigma2[i] ~ gamma(a[i], b[i]);
 //   }
 //   else if (dsigma2[i] == 1){
 //     sigma2[i] ~ inv_gamma(a[i], b[i]);
 //   }
 //   else{
 //     sigma2[i] ~ lognormal(a[i], b[i]);
 //  }
 //}

 grad~normal(linPred, sigR);

 eta~normal(X*betaU,sigU);
 Y~normal(muY+X*betaY,sigYI);
}
