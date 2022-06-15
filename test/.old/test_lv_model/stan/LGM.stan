data {
  int<lower = 0> N; // scalar, number of person times number of items
  int<lower = 0> ntime; // scalar, number of items
  int<lower = 0> nindi; // scalar, number of persons
  int<lower = 0> nfac; // scalar, number of factors
  vector[N] response; // vector, long form of item responses
  // all remaining entries are data in long form
  // with consecutive integers beginning at 1 acting as unique identifiers

  int iidx[N] ;
  int tidx[N] ;

  matrix[N,nfac] time_loading;   // X matrix including intercept
  vector[nfac] gmean; 

}

parameters {
  //vector<lower = 0>[ntime] item_vars; // item vars heteroskedastic
  real<lower=0> item_vars;
  vector[nfac] thetas[nindi]; // person scores for each factor
  //matrix[nindi, nfac] thetas;
  cholesky_factor_corr[nfac] L; // Cholesky decomp of corr mat of random slopes
  
}

transformed parameters {
  vector[N] yhat;
  //vector[N] item_sds_i;

  for (i in 1:N) {
    //yhat[i] = time_loading[tidx[i],] * thetas[iidx[i], ];
	
	yhat[i] = thetas[iidx[i], 1] + (tidx[i]-1) * thetas[iidx[i], 2];

	// operator*(matrix x, matrix y)
    //yhat[i] = alphas[items[i]] * thetas[persons[i], factors[i]] + betas[items[i]];
    //item_sds_i[i] = sqrt(item_vars[tidx[i]]);
  }
}

model {
  vector[nfac] A = rep_vector(1, nfac); // Vector of random slope variances
  matrix[nfac, nfac] A0;
  
  L ~ lkj_corr_cholesky(nfac);
  A0 = diag_pre_multiply(A, L);
  //thetas ~ multi_normal_cholesky(rep_vector(0, nfac), A0);
  thetas ~ multi_normal_cholesky(gmean, A0);
  
  item_vars ~ inv_gamma(1, 1);

  response ~ normal(yhat, item_vars);
}
// last line
