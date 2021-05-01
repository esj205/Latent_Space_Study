data {
  int<lower=0> N;
  vector[N] x; //predictor 
  vector[N] y; //response
}


parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}


model {
  y ~ normal(alpha + beta*x, sigma);
}

