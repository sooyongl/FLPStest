data{
  //data infromation
  int<lower=1> nsecWorked;
  
  int<lower=1> nstud;
  int<lower=1> nsec;
  int<lower=0> nfac; // scalar, number of factors
  int<lower=2> max_k;                             // Max category
 
  // indices
  int<lower=1,upper=nstud> studentM[nsecWorked];
  int<lower=1,upper=nsec> section[nsecWorked];
  
  // index for factor loadings --------------
  matrix[nsec, nfac] factoridx;
  int<lower=0> firstitem[nsec];
  
  // data data
  int<lower=1,upper=max_k> grad[nsecWorked];      // long-format data
}

parameters{
  
  vector[nfac] eta[nstud]; // person scores for each factor
  cholesky_factor_corr[nfac] L; // Cholesky decomp of corr mat of random slopes
  
  matrix[nsec, nfac] disc_free;
  //real diff_free[nsec, nax_k-1];
  vector[max_k-1] diff_free[nsec]; //item category intercept
  
  // mean growth
  //vector[nfac] gm;
}

transformed parameters {
  //real linPred[nsecWorked];
  matrix[max_k, nsecWorked] p;      // probs of reponse
  matrix[max_k, nsecWorked] s;      // logits of reponse
  
  matrix[nsec, nfac] disc;
  vector[max_k-1] diff[nsec]; //item category intercept
  
  for(jjj in 1:nfac) {
  
    for(jj in 1:nsec) {
	  if(factoridx[jj, jjj] != 0) {
        if(firstitem[jj] == 1) {
          disc[jj, jjj] = 1;
		  diff[jj, 1] = 0;
		  	for(kk in 2:(max_k-1)) {
	          diff[jj, kk] = diff_free[jj, kk];
	        }
	  
        } else {
          disc[jj, jjj] = disc_free[jj, jjj];
		  diff[jj] = diff_free[jj];
        }
      } else {
        disc[jj, jjj] = 0;
		diff[jj] = diff_free[jj];
      }
    }
  }
  
  for(i in 1:nsecWorked) {
    s[1,i] = 0; //reference
    for(k in 2:max_k) {
	  s[k,i] = s[k-1, i] + diff[section[i], k-1] + disc[section[i], 1:nfac] * eta[studentM[i]];
	}
	
    p[,i] = softmax(s[,i]);
	
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
  
  for (i in 1:nsecWorked){
    grad[i] ~ categorical(p[,i]);
  }
  eta ~ multi_normal_cholesky(rep_vector(0, nfac), A0);
  
}
// last line blank