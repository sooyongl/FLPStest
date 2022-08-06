data{
  //data infromation
  int<lower=1> nsecWorked;
  int<lower=1> ncov;
  int<lower=1> nstud;
  int<lower=1> nsec;
  int<lower=0> nfac; // scalar, number of factors
 
  // indices
  int<lower=1,upper=nstud> studentM[nsecWorked];
  int<lower=1,upper=nsec> section[nsecWorked];

  // data data
  real grad[nsecWorked];
  matrix[nstud,ncov] X;
  int<lower=0,upper=1> Z[nstud];
  real Y[nstud];

  // prior information
  matrix[nsec,nfac] time_loading; // X matrix including intercept
  vector[nfac] gmean; 
}

parameters{

 vector[nfac] eta[nstud]; // person scores for each factor
 cholesky_factor_corr[nfac] L; // Cholesky decomp of corr mat of random slopes
 
 vector[ncov] betaU1;
 vector[ncov] betaU2;
 
 vector[ncov] betaY;

 //real muEta;
 real b00;
 real a11;
 real a12;
 //vector[nfac] a1;
 real b0;
 
 //vector[nfac] b1;
 real b11;
 real b12;

 // mean growth
 vector[nfac] gm;

 real<lower=0> sigR;
 real<lower=0> sigY[2];
}

transformed parameters {
  real linPred[nsecWorked];
  
  for (i in 1:nsecWorked) {
	linPred[i] = eta[studentM[i], 1] + (time_loading[section[i],2]) * eta[studentM[i], 2];
  }
}

model{
 vector[nstud] muY;
 real useEff[nstud];
 real trtEff[nstud];
 real sigYI[nstud];
 vector[nfac] A = rep_vector(1, nfac); // Vector of random slope variances
 matrix[nfac, nfac] A0;  
 vector[nfac] ggmean; 
  
 for(i in 1:nstud){
  useEff[i]= a11*eta[i, 1] + a12*eta[i, 2];
  trtEff[i]=b0 + b11*eta[i, 1] + b12*eta[i, 2];
  
  muY[i]=b00+useEff[i]+Z[i]*trtEff[i];
  sigYI[i]=sigY[Z[i]+1];
 }
 
 ggmean[1] = gm[1] + mean(X*betaU1);
 ggmean[2] = gm[2] + mean(X*betaU2);
 
 L ~ lkj_corr_cholesky(nfac);
 A0 = diag_pre_multiply(A, L);
 
 sigR~inv_gamma(2, 1); //sigR ~ cauchy(0, 2.5);
 betaY~normal(0,2);
 betaU1[1]~normal(1,1);
 betaU1[2]~normal(-1,1);
 
 betaU2[1]~normal(1,1);
 betaU2[2]~normal(-1,1);
 
 b00~normal(0,2);
 
 a11~normal(0,1);
 a12~normal(0,1);
 
 b0~normal(0,1);
 
 b11~normal(0,1);
 b12~normal(0,1);

 for(ifac in 1:nfac) {
   gm[ifac] ~ normal(gmean[ifac],1);
 };

 grad~normal(linPred, sigR);
 eta ~ multi_normal_cholesky(ggmean, A0);
 Y~normal(muY+X*betaY,sigYI);
}
// last line blank