stan<-
"data {
  real g_alpha; // for inverse gamma
  real g_beta; // for inverse gamma
  int<lower = 0> N; // scalar, number of person times number of items
  int<lower = 0> Ni; // scalar, number of items
  int<lower = 0> Np; // scalar, number of persons
  int<lower = 0> Nf; // scalar, number of factors
  vector[N] response; // vector, long form of item responses
  // all remaining entries are data in long form
  // with consecutive integers beginning at 1 acting as unique identifiers
  int<lower = 1, upper = Ni> items[N];
  int<lower = 1, upper = Np> persons[N];
  int<lower = 1, upper = Nf> factors[N];
}

parameters {
  vector<lower = 0>[Ni] item_vars; // item vars heteroskedastic
  real<lower = 0> sigma_alpha; // sd of loadings, hyperparm
  vector<lower = 0>[Ni] alphas; // loadings
  vector[Ni] betas; // item intercepts, default uniform prior
  vector[Nf] thetas[Np]; // person scores for each factor
  cholesky_factor_corr[Nf] L; // Cholesky decomp of corr mat of random slopes
}

transformed parameters {
  vector[N] yhat;
  vector[N] item_sds_i;

  for (i in 1:N) {
    yhat[i] = alphas[items[i]] * thetas[persons[i], factors[i]] + betas[items[i]];
    item_sds_i[i] = sqrt(item_vars[items[i]]);
  }
}

model {
  vector[Nf] A = rep_vector(1, Nf); // Vector of random slope variances
  matrix[Nf, Nf] A0;

  L ~ lkj_corr_cholesky(Nf);
  A0 = diag_pre_multiply(A, L);
  thetas ~ multi_normal_cholesky(rep_vector(0, Nf), A0);

  alphas ~ lognormal(0, sigma_alpha);
  sigma_alpha ~ cauchy(0, 2.5); // hyperparm
  item_vars ~ inv_gamma(g_alpha, g_beta);

  response ~ normal(yhat, item_sds_i);
}
/////
"


library(lavaan)

dat <- HolzingerSwineford1939
dat$sex <- dat$sex - 1 # Who is 1, no idea
dat$grade <- dat$grade - min(dat$grade, na.rm = TRUE)
dat$ID <- 1:nrow(dat)

# Make data long
dat.l <- tidyr::gather(dat, item, score, x1:x9)
dat.l$item.no <- as.integer(gsub("x", "", dat.l$item))

dat.l$Fs <- ((dat.l$item.no - 1) %/% 3) + 1

data = list(
  N = nrow(dat.l),
  Ni = length(unique(dat.l$item)),
  Np = length(unique(dat.l$ID)),
  Nf = length(unique(dat.l$Fs)),
  items = dat.l$item.no,
  factors = dat.l$Fs,
  persons = dat.l$ID,
  response = dat.l$score,
  g_alpha = 1,
  g_beta = 1
)


library(rstan)

options(mc.cores = parallel::detectCores()) # Use multiple cores
rstan_options(auto_write = TRUE) # One time Stan program compilation

cfa.mm <- stan_model(stanc_ret = stanc(file = "bayes_script/cfa.stan")) # Compile Stan code

cfa.stan.fit <- sampling(
  cfa.mm, data = list(
    N = nrow(dat.l), Ni = length(unique(dat.l$item)),
    Np = length(unique(dat.l$ID)), Nf = length(unique(dat.l$Fs)),
    items = dat.l$item.no, factors = dat.l$Fs,
    persons = dat.l$ID, response = dat.l$score,
    g_alpha = 1, g_beta = 1),
  control = list(adapt_delta = .99999, max_treedepth = 15))



