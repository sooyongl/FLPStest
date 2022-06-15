data {
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
parameters {
  vector[nstud] eta ; // latent trait
  real alpha_free [nsec] ; // slope
  //ordered[K-1] ka[n_item]; //item category intercept
  ordered[max_k-1] ka_free[nsec]; //item category intercept
}

transformed parameters {
  real alpha [nsec] ; // slope
  ordered[max_k-1] ka[nsec];

  for(i in 1:nsec){
    if(i == 1){
	  alpha[i] = 1;
	  ka[i, 1] = 0;
	  for(kk in 2:(max_k-1)) {
	    ka[i ,kk] = ka_free[i, kk];
	  }
	} else {
	  alpha[i] = alpha_free[i];
	  for(kk in 1:(max_k-1)) {
	    ka[i, kk] = ka_free[i, kk];
	  }
	}
  }
}

model{
  eta ~ normal(0,1);
  alpha_free~ normal(1,1);
  for (j in 1:nsec){
    for (k in 1:(max_k-1)){
      ka_free[j,k]~normal(0,1);
    }
  }
  for (i in 1:nsecWorked){
    
      grad[i]~ordered_logistic(eta[studentM[i]]*alpha[section[i]],ka[section[i]]);
    
  }
}