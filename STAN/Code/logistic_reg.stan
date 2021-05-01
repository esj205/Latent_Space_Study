data {
  int<lower=0> N; // number of obs
  int<lower=0> K; // number of predictors
  matrix[N, K] x; // predictor design matrix
  int y[N]; // outcome //different from normal or linear regression.
  
}

parameters {
  real alpha; // intercept
  vector[K] beta; // coefficients for predictors
}

transformed parameters{ // we need this step for bernoulli_logit
  vector[N] eta;
  eta = alpha + x * beta;
}

model {
  y ~ bernoulli_logit(eta) ; // likelihood
}


