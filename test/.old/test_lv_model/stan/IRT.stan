data{
  // IRT model
  // int<lower=1> type_model;

  //Sample sizes
  int<lower=1> nsecWorked;                        // number of rows in long-format data
  int<lower=1> nstud;                             // number of students (I)
  int<lower=1> nsec;                              // number of items    (J)

  // indices
  int<lower=1,upper=nstud> studentM[nsecWorked];  // student index for long-format data
  int<lower=1,upper=nsec> section[nsecWorked];    // item index for long-format data

  // data data
  int<lower=0,upper=1> grad[nsecWorked];      // long-format data

}

parameters{
  vector[nstud] eta;
  real disc_free[nsec];
  real diff_free[nsec];
}

transformed parameters {
  real linPred[nsecWorked];

  vector[nsec] disc;
  vector[nsec] diff;
	
  for(j in 1:nsecWorked) {
    if(section[j] == 1){
      disc[section[j]] = 1;
      diff[section[j]] = 0;
    } else{
      disc[section[j]] = disc_free[section[j]];
      diff[section[j]] = diff_free[section[j]];
    }
    linPred[j] = diff[section[j]] + disc[section[j]] * eta[studentM[j]];
}
}
model{
  disc_free ~ uniform(-10, 10);
  diff_free ~ uniform(-10, 10);  
  eta ~ normal(0, 1);

  grad ~ bernoulli_logit(linPred);
}

generated quantities {
  
  real difficulty[nsec];
  
  for(jj in 1:nsec) {
    difficulty[jj] = - diff[jj] / disc[jj];
  }

}