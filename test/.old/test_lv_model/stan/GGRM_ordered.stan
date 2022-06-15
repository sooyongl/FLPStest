data {
  int <lower=0> n_person; // number of persons
  int <lower=0> n_item; // number of items
  int <lower=2> K; //number of categories for each item
  int <lower=1,upper=K> Y[n_person,n_item]; // where number in the braket needs to be integer only!
}
parameters {
  vector[n_person] eta ; // latent trait
  real alpha_free [n_item] ; // slope
  //ordered[K-1] ka[n_item]; //item category intercept
  ordered[K-1] ka_free[n_item]; //item category intercept
}

transformed parameters {
  real alpha [n_item] ; // slope
  ordered[K-1] ka[n_item];

  for(i in 1:n_item){
    if(i == 1){
	  alpha[i] = 1;
	  ka[i, 1] = 0;
	  for(kk in 2:(K-1)) {
	    ka[i ,kk] = ka_free[i, kk];
	  }
	} else {
	  alpha[i] = alpha_free[i];
	  for(kk in 1:(K-1)) {
	    ka[i, kk] = ka_free[i, kk];
	  }
	}
  }
}

model{
  eta ~ normal(0,1);
  alpha_free~ normal(0,1);
  for (j in 1:n_item){
    for (k in 1:(K-1)){
      ka_free[j,k]~normal(0,1);
    }
  }
  for (i in 1:n_person){
    for (j in 1:n_item){
      Y[i,j]~ordered_logistic(eta[i]*alpha[j],ka[j]);
    }
  }
}