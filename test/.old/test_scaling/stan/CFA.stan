data{
//Sample sizes
 int<lower=1> nsecWorked;
 int<lower=1> nstud;
 int<lower=1> nsec;

// indices
 int<lower=1,upper=nstud> studentM[nsecWorked];
 int<lower=1,upper=nsec> section[nsecWorked];

// data data
 real grad[nsecWorked];
}

parameters{
 vector[nstud] eta;
 real lambda[nsec];
 real<lower=0> sigR;
}

transformed parameters {
 vector[nsec] lambda1;
 real linPred[nsecWorked];
 
  for(j in 1:nsecWorked) {
    if(section[j] == 1)
      lambda1[section[j]] = 1;
    else
      lambda1[section[j]] = lambda[section[j]];

    linPred[j] = lambda1[section[j]] * eta[studentM[j]];
  }
}

model{

 eta ~ normal(0, 1);
 lambda ~ uniform(-1.5, 1.5) ; //normal(0, sqrt(1));
 sigR ~ inv_gamma(2.1, 1.1);

 grad ~ normal(linPred, sigR);
}
