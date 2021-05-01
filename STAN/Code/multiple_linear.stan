data {
  int<lower=0> N; // number of obs
  int<lower=0> K; // number of predictors
  matrix[N, K] x; // predictor matrix
  vector[N] y; // outcome vector
}

parameters {
  real alpha; // intercept
  vector[K] beta; // coefficients for predictors
  real<lower=0> sigma; // error scale
}

model {
  y ~ normal(x*beta + alpha, sigma); // likelihood
}

