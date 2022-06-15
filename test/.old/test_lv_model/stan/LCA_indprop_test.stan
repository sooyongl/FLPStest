//https://discourse.mc-stan.org/t/latent-class-model-estimation-with-long-format-data/1799
data {
  int<lower=1> nsecWorked;    // number of rows in long-format data
  int<lower=1> nsec;          // number of items
  int<lower=1> nstud;         // number of respondents

  int studentM[nsecWorked];   // student index for long-format data
  int section[nsecWorked];    // item index for long-format data

  int<lower=1,upper=2> grad[nsecWorked];      // long-format data

  int<lower=1> nclass;                        // number of latent class
  
  matrix[nstud,ncov] X;
  int<lower=1> ncov;
}

parameters {
  real alpha;
  vector[nstud] studEff;

  real <lower = 0, upper = 1> p[nclass, nsec]; // probs of reponse = 1
  
   //real muEta;
  vector[nclass] b00;
  vector[nclass] a1;
  //vector[nclass] b0;
  vector[nclass] b1;
  
  vector[ncov] betaY;
 
  real<lower=0> sigY[2];
}

transformed parameters {
  vector[nstud] nu=inv_logit(alpha + studEff);
}

model{
  vector[nclass] ps;
  matrix[nstud, nclass] muY;
  real sigYI[nstud];
  
   for(w in 1:nsecWorked) {
    
	int i = studentM[w]
	
	muY[i, c] = X*betaY;
	sigYI[i]=sigY[Z[i]+1];

   
    for (c in 1:nclass) {
        real log_nu;	  	
		
		if(c == 1) {
		   log_nu = log(nu[studentM[w]]);
    	} else {
	       log_nu = log(1-nu[studentM[w]]);
		}
		
		ps[c] = log_nu + grad[w] * log(p[c,section[w]]) + (1 - grad[w]) * log(1 - p[c,section[w]]);
	
        muY[i,c] = muY[i, c] + b00[c] + b1[c]*Z[i] + a1[c];	
	}

	target += log_sum_exp(ps);
   }
   
   Y~normal(muY[,1],sigYI);
   Y~normal(muY[,2],sigYI);
}
//last line empty