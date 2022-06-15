# https://bookdown.org/marklhc/notes_bookdown/missing-data.html
# https://gist.github.com/rmcelreath/9406643583a8c99304e459e644762f82

# "impute" missing binary predictor
# really just marginalizes over missingness
# imputed values produced in generated quantities

N <- 1000 # number of cases
N_miss <- 100 # number missing values
x_baserate <- 0.25 # prob x==1 in total sample
a <- 0 # intercept in y ~ N( a+b*x , 1 )
b <- 1 # slope in y ~ N( a+b*x , 1 )

# simulate data
x <- sample( 0:1 , size=N , replace=TRUE , prob=c(1-x_baserate,x_baserate) )
i_miss <- sample( 1:N , size=N_miss )
x_obs <- x
x_obs[i_miss] <- (-1)
x_NA <- x_obs
x_NA[i_miss] <- NA
x_miss <- ifelse( 1:N %in% i_miss , 1 , 0 )
y <- rnorm( N , a + b*x , 1 )

m_code <- "
data{
    int N;
    int x[N];
    int x_miss[N];
    real y[N];
}
parameters{
    real a;
    real b;
    real<lower=0,upper=1> x_mu;
}
model{
    a ~ normal(0,10);
    b ~ normal(0,1);
    x_mu ~ beta(1,1);
    for ( i in 1:N ) {
        if ( x_miss[i]==1 ) {
            // x missing
            target += log_mix( x_mu ,
                    normal_lpdf( y[i] | a + b , 1 ),
                    normal_lpdf( y[i] | a , 1 )
                );
        } else {
            // x not missing
            x[i] ~ bernoulli(x_mu);
            y[i] ~ normal( a + b*x[i] , 1 );
        } 
    }//i
}//model
generated quantities{
    vector[N] x_impute;
    for ( i in 1:N ) {
        real logPxy;
        real logPy;
        if ( x_miss[i]==1 ) {
            // need P(x|y)
            // P(x|y) = P(x,y)/P(y)
            // P(x,y) = P(x)P(y|x)
            // P(y) = P(x==1)P(y|x==1) + P(x==0)P(y|x==0)
            logPxy = log(x_mu) + normal_lpdf(y[i]|a+b,1);
            logPy = log_mix( x_mu ,
                    normal_lpdf( y[i] | a + b , 1 ),
                    normal_lpdf( y[i] | a , 1 ) );
            x_impute[i] = exp( logPxy - logPy );
        } else {
            x_impute[i] = x[i];
        }
    }//i
}//gq
"

library(rethinking)
m <- stan( 
  model_code=m_code , 
  data=list(N=N,y=y,x=x_obs,x_miss=x_miss),
  chains=1 )

precis(m)

# show imputed medians
post <- extract.samples(m)
Px <- apply(post$x_impute,2,median)
pt1 <- mean( Px[x_miss==1 & x==1] )
pt0 <- mean( Px[x_miss==1 & x==0] )
plot( x[x_miss==1] , Px[x_miss==1] , ylim=c(0,1) , xlab="true" , ylab="imputed probability == 1" )
points( c(0,1) , c(pt0,pt1) , pch=16 , col="red" )