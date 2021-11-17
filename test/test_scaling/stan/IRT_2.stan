data {
  int N;
  int J;
  int grad[N, J];
  
}

parameters {
  vector[J] difficulty; // Now per-item discrimination
  vector[J] discrim;    // Now per-item discrimination
  vector[N] eta; // individual ability score
}

model {
  matrix[N, J] linPred;
  
  // priors
  eta ~ normal(0, 1);
  discrim    ~ uniform(-10, 10);
  difficulty ~ uniform(-10, 10);
  
  //discrim    ~ student_t(3, 0, 5);
  //difficulty ~ student_t(3, 0, 5);
  
  for (j in 1:J){
    linPred[,j] = discrim[j] * (eta - difficulty[j]);
  }
  
  // likelihood
  for (j in 1:J)  grad[,j] ~ bernoulli_logit(linPred[,j]);
  
}
