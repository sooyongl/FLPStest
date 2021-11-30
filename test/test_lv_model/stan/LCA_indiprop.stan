//https://discourse.mc-stan.org/t/latent-class-model-estimation-with-long-format-data/1799
data {
  int<lower=1> nsecWorked;                   // number of rows in long-format data
  int<lower=1> nsec;                         // number of items
  int<lower=1> nstud;                        // number of respondents

  int<lower=1> studItem[nstud];
  int<lower=0> cumStudItem[nstud];           // cummulative sum of item administred

  int studentM[nsecWorked];  // student index for long-format data
  int section[nsecWorked];    // item index for long-format data

  int<lower=0,upper=1> grad[nsecWorked];      // long-format data

  int<lower=1> nclass;                        // number of latent class
}

parameters {
  //simplex[nclass] nu[nstud];                   // overall class proportion
  //matrix<lower=0, upper=1>[nclass, nstud] nu;                          // overall class proportion
  real nu[nclass-1, nstud];
  
  // real nu_logit[nclass];                    // overall class logit
  real <lower = 0, upper = 1> p[nclass, nsec]; // probs of reponse = 1

  // real class_logit[nstud] ;                 // individual class logits
  // simplex[nclass] indi_nu[nstud];
}

//transformed parameters {
  // int class_assign[nstud] ;                 // individual class assign
//}

model{
  matrix[nclass, nstud] ps;
  //matrix[nclass, nstud] log_nu = log(nu);
  //real <lower = 0, upper = 1> log_nu[nclass, nsec]; // probs of reponse = 1
  
 
  for (j in 1:nstud) {
    int nitem_given = studItem[j];
    
	for (c in 1:nclass) {

	  real log_items[nitem_given]; 

	  for (m in 1:nitem_given) {
	    
		int i = section[cumStudItem[j] + m];

		log_items[m] = grad[cumStudItem[j] + m] * log(p[c,i]) + (1 - grad[cumStudItem[j] + m]) * log(1 - p[c,i]);

		}
        
		if(c == 1) {
		  real cp = 1 / (1 + exp(sum(nu[,j])));
		  ps[c,j] = log(cp) + sum(log_items);
		} else {
		  int cc = c - 1;
		  real cp = exp(nu[cc,j]) / (1 + exp(sum(nu[,j])));
		  
		  ps[c,j] = log(cp) + sum(log_items);
		}
		
        
	}

	target += log_sum_exp(to_vector(ps[,j]));

  }
}
//last line empty