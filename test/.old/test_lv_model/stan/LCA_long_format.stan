//https://discourse.mc-stan.org/t/latent-class-model-estimation-with-long-format-data/1799
data {
  int<lower=1> I;                                     // number of items
  int<lower=1> J;                                     // number of respondents
  int<lower=1> N;                                     // number of observations
  int<lower=1,upper=I> ii[N];                         // item for obs n
  int<lower=1,upper=J> jj[N];                         // respondent for obs n
  int<lower=0,upper=1> y[N];                          // score for obs n
}
parameters {
  simplex[2] nu;

  // average intercept and main effect
  real mean_intercept;
  real<lower=0> mean_maineffect;

  // item level deviations from average effects
  real dev_intercept[I];
  real<lower=-mean_maineffect> dev_maineffect[I];
}
transformed parameters {
  vector[2] log_nu = log(nu);

  real intercept[I];
  real maineffect[I];
  vector[I] master_pi;
  vector[I] nonmaster_pi;

  for (i in 1:I) {
    intercept[i] = mean_intercept + dev_intercept[i];
    maineffect[i] = mean_maineffect + dev_maineffect[i];

    nonmaster_pi[i] = inv_logit(intercept[i]);
    master_pi[i] = inv_logit(intercept[i] + maineffect[i]);
  }
}
model{
  real ps[2];
  //real log_items[N];
  matrix[I,2] pi;

  // Priors
  mean_intercept ~ normal(0, 5);
  mean_maineffect ~ normal(0, 5);

  dev_intercept ~ normal(0, 3);
  dev_maineffect ~ normal(0, 3);

  // Probability of correct response for each class on each item
  for (c in 1:2) {
    for (i in 1:I) {
      pi[i,c] = master_pi[i]^(c - 1) * nonmaster_pi[i]^(1 - (c - 1));
    }
  }

  // Define model
  for (j in 1:J) {
    for (c in 1:2) {

	  real log_items[l[j]]; // 사람별로 아이템 수

	  for (m in 1:l[j]) { // 아이템 10개면

        int i = ii[s[j] + m - 1];

	    // 1번 사람의 1번 문항에 응답값
		// s 는 몇번째 y 값을 써야 하는지를 알려주는 index
		//
        log_items[m] = y[s[j] + m - 1] * log(pi[i,c]) + (1 - y[s[j] + m - 1]) * log(1 - pi[i,c]);


      }
      ps[c] = log_nu[c] + sum(log_items);
    }
    target += log_sum_exp(ps);
  }

  // data
  // int l; // 여기에는 응답 총 몇개식 했는지 들어가 있어야 함.

    for (j in 1:J) { //nstud

	  // studentM[j]
	  int item_list = idx[j, ]
	  int resp_idx = y[j, 3]

	  for (c in 1:2) {

		int len_item row(item_list); # j 가 응답한 아이템 수 row column size ...

		// real log_items[l[j]]; // 사람별로 아이템 수 혹은 아이템 넘버. size(l[j, ]) 하면 length임.
	    real log_items[len_item]; // 사람별로 아이템 수 혹은 아이템 넘버. size(l[j, ]) 하면 length임.

		//for (m in 1:l[j]) {
		  for (m in 1:len_item) {

		  //int ii = idx[s[j] + m - 1];
		  //log_items[m] = y[] * log(pi[i,c]) + (1 - y[]) * log(1 - pi[i,c]);

		  int i = len_item[m];
		  log_items[m] = y[resp_idx[m]] * log(pi[i,c]) + (1 - y[resp_idx[m]]) * log(1 - pi[i,c]);

		}


	  }
	}

}
