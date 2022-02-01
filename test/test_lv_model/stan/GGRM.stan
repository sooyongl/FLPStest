functions {
  real rsm(int y, real theta, real beta, vector kappa) {
    vector[rows(kappa) + 1] unsummed;
    vector[rows(kappa) + 1] probs;
    unsummed = append_row(rep_vector(0, 1), theta - beta - kappa);
    probs = softmax(cumulative_sum(unsummed));
    return categorical_lpmf(y + 1 | probs);
  }
}
data {
  int<lower=1> nsec;                // # items
  int<lower=1> nstud;                // # persons
  int<lower=1> nsecWorked;                // # responses
  int<lower=1,upper=nsec> section[nsecWorked];    // i for n
  int<lower=1,upper=nstud> studentM[nsecWorked];    // j for n
  int<lower=0> grad[nsecWorked];             // response for n; y in {0 ... m_i}
  int<lower=1> max_k;                // # person covariates
  //matrix[J,K] W;                 // person covariate matrix
}
transformed data {
  int m;                         // # steps
  m = max_k - 1;
}
parameters {
  vector[nsec] alpha;
  vector[nsec-1] beta_free;
  vector[m-1] kappa_free;
  vector[nstud] theta;
}
transformed parameters {
  vector[nsec] beta;
  vector[m] kappa;
  beta = append_row(beta_free, rep_vector(-1*sum(beta_free), 1));
  kappa = append_row(kappa_free, rep_vector(-1*sum(kappa_free), 1));
}
model {
  alpha ~ normal(1, 1);
  beta_free ~ normal(0, 2);
  kappa_free ~ normal(0, 2);
  theta ~ normal(0, 1);
  for (n in 1:nsecWorked)
    target += rsm(grad[n], theta[studentM[n]] .* alpha[section[n]], beta[section[n]], kappa);
}

