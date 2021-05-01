data {
  int N;
  int<lower = 0, upper =1> y[N];
}

parameters {
  real<lower = 0, upper = 1> p;
}

model {
  for(n in 1:N)
    y[n] ~ bernoulli(p); // likelihood
    // y ~ bernoulli(p);
    p ~ beta(1,1); //prior (non-informative)
}

