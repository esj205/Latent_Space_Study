data {
  int K; // possible outcomes
  int N; // number of obs
  int D; // number of predictors(including intercept)
  int y[N];
  matrix[N, D] x;
}


parameters {
  matrix[D, K] beta;
}

model {
  matrix[N,K] x_beta = x * beta;
  
  to_vector(beta) ~ normal(0,5); //prior for beta
  
  //As of Stan 2.18, the categorical-logit distribution is not vectorized for parameter arguments,
  //so the loop is required.
  for (n in 1:N)
    y[n] ~ categorical_logit(x_beta[n]'); // x_beta[n]'is the transpose of x_beta[n]
}

