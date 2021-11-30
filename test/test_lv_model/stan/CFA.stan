data{
//Sample sizes
 int<lower=1> nsecWorked;              // number of rows in long-format data
 int<lower=1> nstud;                   // number of students (I)
 int<lower=1> nsec;                    // number of items    (J)

// indices
 int<lower=1,upper=nstud> studentM[nsecWorked];    // student index for long-format data
 int<lower=1,upper=nsec> section[nsecWorked];      // item index for long-format data

// data data
 real grad[nsecWorked];                 // long-format data
}

parameters{
 vector[nstud] eta;
 real lambda[nsec];
 real<lower=0> sigR;
}

transformed parameters {
 vector[nsec] lambda1;
 real linPred[nsecWorked];
 
 for(jj in 1:nsec) {
    if(jj == 1)
      lambda1[1] = 1;
    else
      lambda1[jj] = lambda[jj];
 }
 
  for(j in 1:nsecWorked) {
   // if(section[j] == 1)
   //   lambda1[section[j]] = 1;
   // else
   //   lambda1[section[j]] = lambda[section[j]];

    linPred[j] = lambda1[section[j]] * eta[studentM[j]];
  }
}

model{

 eta ~ normal(0, 1);
 lambda ~ uniform(-1.5, 1.5) ; //normal(0, sqrt(1));
 sigR ~ inv_gamma(2.1, 1.1);

 grad ~ normal(linPred, sigR);
}
