data {
  int<lower=1> J; // number of students
  int<lower=1> K; // number of questions
  int<lower=1> N; // number of observations
  int<lower=1, upper=J> jj[N]; // student for observation n
  int<lower=1, upper=K> kk[N]; // question for observation n
  int<lower=0, upper=1> y[N]; // correctness for observation n
}

parameters {
  real mu_beta; // mean question difficulty
  vector[J] alpha; // ability for student j - mean ability
  vector[K] beta; // difficulty for k-th item
  vector<lower=0>[K] gamma; // discrimination of k
  real<lower=0> sigma_beta; // scale of difficulties
  real<lower=0> sigma_gamma; // scale of log discrimination
}


model {
  alpha ~ std_normal(); // prior of alpha(ability)
  beta ~ normal(0, sigma_beta); // prior of beta(difficulty)
  gamma ~ lognormal(0, sigma_gamma); // prior of discrimination
  mu_beta ~ cauchy(0,5); // prior of mean question difficulty
  sigma_beta ~ cauchy(0,5); // prior of scale of difficulties
  sigma_gamma ~ cauchy(0,5); // prior of scale of log discrimination
  y ~ bernoulli_logit(gamma[kk] .* (alpha[jj] - (beta[kk] + mu_beta))); // likelihood
}

