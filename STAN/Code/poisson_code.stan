data {
int<lower=0> N; // Number of observations
int<lower=0> p; // Number of beta parameters
// Covariates
int <lower=0, upper=1> intercept[N];
int <lower=0, upper=1> age55_59[N];
int <lower=0, upper=1> age60_64[N];
int <lower=0, upper=1> age65_69[N];
int <lower=0, upper=1> age70_74[N];
int <lower=0, upper=1> age75plus[N];
int <lower=0, upper=1> cityHorsens[N];
int <lower=0, upper=1> cityKolding[N];
int <lower=0, upper=1> cityVejle[N];

real offset[N]; //offset
int<lower=0> y[N]; //outcomes
}

parameters {
real beta[p];
}


transformed parameters{
  real lp[N]; 
  real <lower=0> mu[N];
  
  for(i in 1:N){
    //Linear Predictor
    lp[i] = beta[1] + beta[2]*age55_59[i] + beta[3]*age60_64[i] + beta[4]*age65_69[i] + beta[5]*age70_74[i] + beta[6]*age75plus[i]+ beta[7]*cityHorsens[i] + beta[8]*cityKolding[i] + beta[9]*cityVejle[i] + offset[i];
    
    //Mean
    mu[i] = exp(lp[i]);
  }
}

model{
  y ~ poisson(mu);
}

