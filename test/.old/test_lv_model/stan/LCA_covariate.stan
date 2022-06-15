data {
  // data
  int<lower=1> nsecWorked;       // number of rows in long-format data
  int<lower=1> nsec;             // number of items
  int<lower=1> nstud;            // number of respondents

  // data index
  int studentM[nsecWorked];      // student index for long-format data
  int section[nsecWorked];       // item index for long-format data
  int<lower=1> nclass;           // number of latent class
  
  // data data
   int<lower=0,upper=1> grad[nsecWorked];
   matrix[nstud,ncov] X;
   int<lower=0,upper=1> Z[nstud];
   real Y[nstud];
}

parameters {
  real alpha;                          // overall class proportion
  vector[ncov] betaY;
  vector[ncov] betaU;
  
  real <lower = 0, upper = 1> p[nclass, nsec]; // probs of reponse = 1
  
  vector b00[nclass];
  // real a1;
  // real b0;
  // real b1;
  vector b01[nclass];
  vector<lower=0>[nclass] sigY;
}

transformed parameters {
   vector[nstud] nu=inv_logit(alpha + X*betaU);
}

model{
  vector[nclass] log_nu = log(nu);

  for (w in 1:nstud) {

    target += log_sum_exp( 
      log(nu[studentM[w]]) + 
	  bernoulli_lpmf(grad[w] | p[1,section[w]) + 
	  normal_lpdf(Y[w] | b00[1] + b01[1]*Z[w] + X*betaU, sigY[1]) + 
	  
	  log(1-nu[stud[w]])+ 
	  bernoulli_lpmf(grad[w] | p[2,section[w]) + 
	  normal_lpdf(Y[w] | b00[2] + b01[2]*Z[w] + X*betaU, sigY[2])
	);
  }
}
