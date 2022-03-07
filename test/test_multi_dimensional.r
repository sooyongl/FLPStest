https://www.probabilitycourse.com/chapter5/5_3_2_bivariate_normal_dist.php


https://online.stat.psu.edu/stat505/lesson/4/4.2

library(rstan)
bi_m0 <-'

data {
  int K;
  int N;
  int nfac;
  matrix[N, K] resp;
  matrix[nfac, N] muu;

  // index for factor loadings
  matrix[K, nfac] factoridx;
  int<lower=0> firstitem[K];

}

parameters {

  real eta1[N];
  real eta2[N];
  
  real muEta1[N];
  real muEta2[N];

  matrix[K, nfac] lambda_free;

  real<lower = 0> sigma1;
  real<lower = 0> sigma2;
  real<lower = -1, upper = 1> rho0;


}

transformed parameters {
  matrix[K, nfac] lambda;
  real<lower = 0> sig_condition;

  sig_condition = (1-rho0^2)*sigma2;

  // Factor loading constraints
  for(jjj in 1:nfac) {
    for(jj in 1:K) {
      if(factoridx[jj, jjj] != 0) {
        if(firstitem[jj] == 1) { // first loading per factor constrained to 1.
          lambda[jj, jjj] = 1;
        } else {
          lambda[jj, jjj] = lambda_free[jj, jjj];
        }
      } else {
        lambda[jj, jjj] = 0;
      }
    }
  };
}

model {
  matrix[N, K] linPred;


  real mean_eta1 = mean(muEta1);
    muEta1 ~ normal(0, 1);
    muEta2 ~ normal(0, 1);

  for(i in 1:N) {

    real std_eta1 = (muEta1[i] - mean_eta1) / sigma1;
    real mu_condition = muEta2[i] + rho0 * sigma2 * std_eta1;

    eta1[i] ~ normal(muEta1[i], sigma1);
    eta2[i] ~ normal(mu_condition, sig_condition);

    for(j in 1:K) {
      linPred[i, j] = lambda[j,1] * eta1[i] + lambda[j,2] * eta2[i];
      resp[i, j] ~ normal(linPred[i,j], 1);
    }

  }
}
'

a1 <- rstan::stan_model(model_code = bi_m0)

lam <- matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1), ncol = 2)
phi <- matrix(c(1, 0.2, 0.2, 1), ncol = 2)
resi <- diag(1, 8)

sig <- lam %*% phi %*% t(lam) + resi

data <- MASS::mvrnorm(300, mu = rep(0, 8), Sigma = sig)

standata <- list(
  factoridx = lam,
  firstitem = c(1,0,0,0,1,0,0,0),
  N = nrow(data),
  K = ncol(data),
  nfac = 2,
  resp = data,
  muu = t(matrix(0, 300, ncol=2))
)

fit <- rstan::sampling(a1, data = standata)
fit






bi_m1 <-
'
  functions {

    real bivn_lpdf(real x1, real x2, real mu1, real mu2, real sig1, real sig2, real rho) {

      real logl = log( 2*pi() + sig1 + sig2 + 0.5*(1-rho^2) );

      real exp_part = -1/(2*(1-rho^2)) * ( ((x1 - mu1) / sig1)^2 -2*rho * (((x1 - mu1) / sig1))* (((x2 - mu2) / sig2)) + ((x2 - mu2) / sig2)^2);


      return logl + exp_part;

    }
  }
    data {
      int K;
      int N;
      int nfac;
      matrix[N, K] resp;
      matrix[nfac, N] muu;

    }

    parameters {

      matrix[N, nfac] eta;
      real<lower = 0> sigma1;
      real<lower = 0> sigma2;
      real<lower = -1, upper = 1> rho0;


    }

    model {
      matrix[N, K] linPred;
      matrix[N, nfac] muEta;

      for(i in 1:N) {
        target += bivn_lpdf(eta[i,1], eta[i,2], muEta[i,1], muEta[i,2], sigma1, sigma2, rho0);

        for(i in 1:K) {
          linPred[j, i] = lambda[i,1:nfac] * to_vector(eta[j, ]);
	        resp[j, i] ~ normal(linPred[j,i], 1);
        }

      }

    }
'
a1 <- rstan::stan_model(model_code = bi_m1)

lam <- matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1), ncol = 2)
phi <- matrix(c(1, 0.2, 0.2, 1), ncol = 2)
resi <- diag(1, 8)

sig <- lam %*% phi %*% t(lam) + resi

data <- MASS::mvrnorm(300, mu = rep(0, 8), Sigma = sig)

standata <- list(

  N = nrow(data),
  K = ncol(data),
  nfac = 2,
  resp = data,
  muu = t(matrix(0, 300, ncol=2))
)

rstan::sampling(a1, data = standata)



m1 <- "
functions {

 real mvn_cholesky_lpdf(matrix X, matrix mu, matrix L) {
    int K = rows(L);
    int N = rows(X);
    real sqrt_det = -N * sum(log(diagonal(L)));
    real norm_const =  -K * N * 0.5 * log(2 * pi());
    real mahab = sum(columns_dot_self(mdivide_left_tri_low(L, (X' - mu))));

    return norm_const + sqrt_det - 0.5 * mahab;
  }
}

data {
 int K;
 int N;
 int nfac;
 matrix[N, K] resp;
 matrix[nfac, N] muu;
}

parameters {

  matrix[N, nfac] eta;
  matrix[nfac, nfac] mysig;

  matrix[K, nfac] lambda;

}

model {

  matrix[N, K] linPred;

  eta ~ mvn_cholesky_lpdf(muu, mysig);

  for(j in 1:N) {
    for(i in 1:K) {

      linPred[j, i] = lambda[i,1:nfac] * to_vector(eta[j, ]);

	    resp[j, i] ~ normal(linPred[j,i], 1);
    }
	}
}

"

a1 <- rstan::stan_model(model_code = m1)

lam <- matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1), ncol = 2)
phi <- matrix(c(1, 0.2, 0.2, 1), ncol = 2)
resi <- diag(1, 8)

sig <- lam %*% phi %*% t(lam) + resi

data <- MASS::mvrnorm(300, mu = rep(0, 8), Sigma = sig)

standata <- list(

  N = nrow(data),
  K = ncol(data),
  nfac = 2,
  resp = data,
  muu = t(matrix(0, 300, ncol=2))
)

rstan::sampling(a1, data = standata)
# f1 <- rstan::stan(model_code = m1, data = standata)

# -------------------------------------------------------------------------

m1 <- "
functions {

 real mvn_cholesky_lpdf(vector X, vector mu, matrix L) {
    int K = rows(L);
    int N = rows(X);
    real sqrt_det = -N * sum(log(diagonal(L)));
    real norm_const =  -K * N * 0.5 * log(2 * pi());
    real mahab = sum(columns_dot_self(mdivide_left_tri_low(L, (X - mu))));

    return norm_const + sqrt_det - 0.5 * mahab;
  }
}

data {
 int K;
 int N;
 int nfac;
 matrix[N, K] resp;
 matrix[nfac, N] muu;
}

parameters {

  matrix[nfac, N] eta;
  matrix[nfac, nfac] mysig;

  matrix[K, nfac] lambda;

}

model {

  matrix[N, K] linPred;

  for(i in 1:N) {



     eta[, i] ~ mvn_cholesky_lpdf(muu[, i], mysig);
  }

  for(j in 1:N) {
    for(i in 1:K) {

      linPred[j, i] = lambda[i,1:nfac] * eta[, i];

	    resp[j, i] ~ normal(linPred[j,i], 1);
    }
	}
}

"

a1 <- rstan::stan_model(model_code = m1)

lam <- matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1), ncol = 2)
phi <- matrix(c(1, 0.2, 0.2, 1), ncol = 2)
resi <- diag(1, 8)

sig <- lam %*% phi %*% t(lam) + resi

data <- MASS::mvrnorm(300, mu = rep(0, 8), Sigma = sig)

standata <- list(

  N = nrow(data),
  K = ncol(data),
  nfac = 2,
  resp = data,
  muu = t(matrix(0, 300, ncol=2))
)

rstan::sampling(a1, data = standata)



















# -------------------------------------------------------------------------


m1 <- "

parameters{

  vector[nfac] eta[nstud];
  cholesky_factor_corr[nfac] L;

}

model {

    vector[2] A = rep_vector(1, nfac);
    matrix[nfac, nfac] A0;

    L ~ lkj_corr_cholesky(nfac);
    A0 = diag_pre_multiply(A, L);


    eta ~ multi_normal_cholesky(muEta[i], A0);

}


"
