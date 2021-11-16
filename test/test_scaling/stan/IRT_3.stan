data{
  //Sample sizes
  int<lower=1> nsecWorked;
  int<lower=1> nstud;
  int<lower=1> nsec; 
  
  // indices
  int<lower=1,upper=nstud> studentM[nsecWorked];
  int<lower=1,upper=nsec> section[nsecWorked];
  
  // data data
  int<lower=0,upper=1> grad[nsecWorked];
  
}
// 
transformed data {
  int nsec1 = nsec - 1;
}

parameters{
  
  vector[nstud] eta;
  
  real disc[nsec];
  real diff[nsec];
  
}

// transformed parameters {
//   real disc1[nsec];
//   real linPred[nsecWorked];
//   
//   for(j in 1:nsecWorked) {
//     if(section[j] == 1)
//       disc1[section[j]] = 1;
//     else
//       disc1[section[j]] = disc[section[j]];
//     
//     linPred[j] = disc1[section[j]] * (eta[studentM[j]] - diff[section[j]]);
    
//     linPred[j] = lmat[section[j], studentM[j]];   
//   }
// }

model{
  real linPred1[nstud];
  real linPred2[nstud];
  real linPred3[nstud];
  real linPred4[nstud];
  real linPred5[nstud];
  real linPred6[nstud];
  real linPred7[nstud];
  real linPred8[nstud];
  real linPred9[nstud];
  real linPred10[nstud];
  
  eta ~ normal(0, 1);
  disc ~ uniform(-10, 10) ; //normal(0, sqrt(1));
  diff ~ uniform(-10, 10) ;
  // disc    ~ student_t(3, 0, 5);
  // diff    ~ student_t(3, 0, 5);

   for(i in 1:nsecWorked){
     if(section[i] == 1)
       linPred1[i] = disc[section[i]] * (eta[studentM[i]] - diff[section[i]]);
       
      if(section[i] == 2)
       linPred2[i] = disc[section[i]] * (eta[studentM[i]] - diff[section[i]]);
       
      if(section[i] == 3)
       linPred3[i] = disc[section[i]] * (eta[studentM[i]] - diff[section[i]]);
       
      if(section[i] == 4)
       linPred4[i] = disc[section[i]] * (eta[studentM[i]] - diff[section[i]]);
       
      if(section[i] == 5)
       linPred5[i] = disc[section[i]] * (eta[studentM[i]] - diff[section[i]]);
       
      if(section[i] == 6)
       linPred6[i] = disc[section[i]] * (eta[studentM[i]] - diff[section[i]]);
       
      if(section[i] == 7)
       linPred7[i] = disc[section[i]] * (eta[studentM[i]] - diff[section[i]]);
       
      if(section[i] == 8)
       linPred8[i] = disc[section[i]] * (eta[studentM[i]] - diff[section[i]]);
       
      if(section[i] == 9)
       linPred9[i] = disc[section[i]] * (eta[studentM[i]] - diff[section[i]]);
       
      if(section[i] == 10)
       linPred10[i] = disc[section[i]] * (eta[studentM[i]] - diff[section[i]]);

	}
	
  grad ~ bernoulli_logit(linPred);
  
  // for (j in 1:J)  Y[,j] ~ bernoulli_logit(lmat[,j]);
}
