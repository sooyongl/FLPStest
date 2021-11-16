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

model{
  real linPred[nsecWorked];
  
  eta ~ normal(0, 1);
  // disc ~ uniform(-10, 10);
  // diff ~ uniform(-10, 10);

  disc ~ student_t(3, 0, 5);
  diff ~ student_t(3, 0, 5);
  
   for(i in 1:nsecWorked){
     linPred[i] = disc[section[i]] * (eta[studentM[i]] - diff[section[i]]);
	}
	
  grad ~ bernoulli_logit(linPred);
}
