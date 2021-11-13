data{
  //Sample sizes
  int<lower=1> nsecWorked;
  int<lower=1> ncov;
  int<lower=1> nstud;
  int<lower=1> nsec;

  // indices
  int<lower=1,upper=nstud> studentM[nsecWorked];
  int<lower=1,upper=nsec> section[nsecWorked];

  // data data
  int<lower=1,upper=4> grad[nsecWorked];
  matrix[nstud,ncov] X;
  int<lower=0,upper=1> Z[nstud];
  real Y[nstud];

}

parameters{
  // IRT model
  // real delta; // mean ability
  vector[nstud] eta; // ability of student nstud

  real beta1[nsec]; // difficulty of question nsec
  real beta2[nsec]; // difficulty of question nsec
  real beta3[nsec]; // difficulty of question nsec

  real alpha[nsec]; // discrimination of nsec



  // vector[nstud] studEff;
  // real secEff[nsec];

  vector[ncov] betaU;
  vector[ncov] betaY;

  real b00;
  real a1;
  real b0;
  real b1;


  real<lower=0> sigY[2];
  real<lower=0> sigU;
}

model{
  //real linPred[nsecWorked];
  vector[nstud] muY;
  real useEff[nstud];
  real trtEff[nstud];
  real sigYI[nstud];

  matrix[4, nsecWorked] p;
  matrix[4, nsecWorked] s;

  // grad model
  for(i in 1:nsecWorked){

    s[1,i] = 0; //reference
    s[2,i] = alpha[section[i]] * (eta[studentM[i]] - beta1[section[i]]);
    s[3,i] = s[2,i] + alpha[section[i]] * (eta[studentM[i]] - beta2[section[i]]);
    s[4,i] = s[3,i] + alpha[section[i]] * (eta[studentM[i]] - beta3[section[i]]);

    p[,i] = softmax(s[,i]);

  }

  for (i in 1:nsecWorked){
    grad[i] ~ categorical(p[,i]);
  }

  for(i in 1:nstud){
    useEff[i]=a1*eta[i];
    trtEff[i]=b0+b1*eta[i];
    muY[i]=b00+useEff[i]+Z[i]*trtEff[i];
    sigYI[i]=sigY[Z[i]+1];
  }

  //priors
  // IRT priors
  beta1 ~ normal(0, 1);
  beta2 ~ normal(0, 1);
  beta3 ~ normal(0, 1);
  alpha ~ normal(0, 1);
  //alpha ~ lognormal(0.5, 1);

  // PS priors
  betaY~normal(0,2);
  betaU~normal(0,2);
  b00~normal(0,2);
  a1~normal(0,1);
  b0~normal(0,1);
  b1~normal(0,1);

  // Fully Latent Principal Stratification model
  // Latent variable model
  //grad~bernoulli_logit(linPred);
  // Causal model
  eta~normal(X*betaU,sigU);
  Y~normal(muY+X*betaY,sigYI);
}
