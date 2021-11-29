data{
  //Sample sizes
  int<lower=1> nsecWorked;                        // number of rows in long-format data
  int<lower=1> nstud;                             // number of students (I)
  int<lower=1> nsec;                              // number of items    (J)
  int<lower=2> max_k;                             // Max category

  int<lower=1,upper=nstud> studentM[nsecWorked];  // student index for long-format data
  int<lower=1,upper=nsec> section[nsecWorked];    // item index for long-format data

  int<lower=1,upper=max_k> grad[nsecWorked];      // long-format data

}

parameters{

  vector[nstud] eta; // ability of student nstud

  real beta[nsec, max_k-1];          // thresholds parameters
  real alpha[nsec];                  // discrimination of nsec
  
}

transformed parameters{
  
  matrix[max_k, nsecWorked] p;      // probs of reponse
  matrix[max_k, nsecWorked] s;      // logits of reponse
  
  real alpha_1[nsec];               // new disc with the first loading fixed to 1
  real beta_1[nsec, max_k-1];       // new threshold with the first cate of first item fixed to 0
  
  for(i in 1:nsecWorked){
	
    if(section[i] == 1){
	  alpha_1[section[i]] = 1;
	  beta_1[section[i], 1] = 0;
	  for(kk in 2:(max_k-1)) {
	    beta_1[section[i], kk] = beta[section[i], kk];
	  }
	  
	} else {
	  
	  alpha_1[section[i]] = alpha[section[i]];
	  for(kk in 1:(max_k-1)) {
	    beta_1[section[i], kk] = beta[section[i], kk];
	  }
	  
	}

    s[1,i] = 0; //reference
    for(k in 2:max_k) {
	  s[k,i] = s[k-1, i] + beta_1[section[i], k-1] + alpha_1[section[i]] * eta[studentM[i]];
	}
	
    p[,i] = softmax(s[,i]);
  }

}

model{

  eta ~ normal(0, 1);
  alpha ~ uniform(-2, 2);
  for(ii in 2:max_k) {
    beta[ii , ] ~ uniform(-10, 10);
  }
  
  for (i in 1:nsecWorked){
    grad[i] ~ categorical(p[,i]);
  }
}

//generated quantities {
//  
//  real difficulty[nsec];
  
//  for(jj in 1:nsec) {
//    difficulty[jj] = - diff1[jj] / disc1[jj];
//  }

//}