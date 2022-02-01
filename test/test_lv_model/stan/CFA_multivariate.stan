data{
  //data infromation
  int<lower=1> nsecWorked;
  
  int<lower=1> nstud;
  int<lower=1> nsec;
  int<lower=0> nfac; // scalar, number of factors
  
  // indices
  int<lower=1,upper=nstud> studentM[nsecWorked];
  int<lower=1,upper=nsec> section[nsecWorked];
  
  // index for factor loadings --------------
  matrix[nsec, nfac] factoridx;
  int<lower=0> firstitem[nsec];
  
  // data data
  real grad[nsecWorked];
}

parameters{
  
  vector[nfac] eta[nstud]; // person scores for each factor
  cholesky_factor_corr[nfac] L; // Cholesky decomp of corr mat of random slopes
  
  //real disc_free[nsec];
  //real diff_free[nsec];
  //vector[nsec] lambda_free;
  matrix[nsec, nfac] lambda_free;
  
  // mean growth
  //vector[nfac] gm;
  
  real<lower=0> sigR;
  
}

transformed parameters {
  //vector[nsec] lambda;
  matrix[nsec, nfac] lambda;
  real linPred[nsecWorked];
  
  for(jjj in 1:nfac) {
  
    for(jj in 1:nsec) {
	  if(factoridx[jj, jjj] != 0) {
        if(firstitem[jj] == 1) {
          lambda[jj, jjj] = 1;
        } else {
          lambda[jj, jjj] = lambda_free[jj, jjj];
        }
      } else {
        lambda[jj, jjj] = 0;
      }
    }
  }
  
  for(j in 1:nsecWorked) {
    
    linPred[j] = lambda[section[j], 1:nfac] * eta[studentM[j]];
  }
  
  //  for (i in 1:nsecWorked) {
    //	linPred[i] = eta[studentM[i], 1] + (time_loading[section[i],2]) * eta[studentM[i], 2];
    //  }
}

model{
  
  vector[nfac] A = rep_vector(1, nfac); // Vector of random slope variances
  matrix[nfac, nfac] A0;  
  //vector[nfac] ggmean;
  
  //ggmean[1] = gm[1];
  //ggmean[2] = gm[2];
  
  L ~ lkj_corr_cholesky(nfac);
  A0 = diag_pre_multiply(A, L);
  
  //for(ifac in 1:nfac) {
    //  gm[ifac] ~ normal(gmean[ifac],1);
    //};
  
  grad~normal(linPred, sigR);
  eta ~ multi_normal_cholesky(rep_vector(0, nfac), A0);
  
}
// last line blank