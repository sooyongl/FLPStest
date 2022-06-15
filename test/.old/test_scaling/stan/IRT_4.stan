data{
  //Sample sizes
  int<lower=1> nsecWorked;
  int<lower=1> nstud;
  int<lower=1> nsec;

  // indices
  int<lower=1,upper=nstud> studentM[nsecWorked];
  int<lower=1,upper=nsec> section[nsecWorked];

  // data data
  int<lower=0,upper=1> grad[nsecWorked];

}

parameters{
  vector[nstud] eta;
  real disc[nsec];
  real diff[nsec];
}

transformed parameters {
 vector[nsec] disc1;
 vector[nsec] diff1;

 real linPred[nsecWorked];

  for(j in 1:nsecWorked) {
    if(section[j] == 1){
      disc1[section[j]] = 1;
      diff1[section[j]] = 0;
    } else{
      disc1[section[j]] = disc[section[j]];
      diff1[section[j]] = diff[section[j]];
    }
    linPred[j] = diff1[section[j]] + disc1[section[j]] * eta[studentM[j]];
  }
}

model{

  eta ~ normal(0, 1);
  disc ~ uniform(-10, 10);
  diff ~ uniform(-10, 10);

  grad ~ bernoulli_logit(linPred);
}

generated quantities {
  
  real difficulty[nsec];
  
  for(jj in 1:nsec) {
    difficulty[jj] = - diff1[jj] / disc1[jj];
  }

}
