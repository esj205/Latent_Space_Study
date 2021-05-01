data {
  int<lower=0> N; // # obs.
  real y[N];
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  y ~ normal(mu, sigma); //likelihood
}

//예측 분포를 생성하고 싶을 때 주로 사용
generated quantities{
  real y_pred;
  y_pred = normal_rng(mu, sigma); //rng = random number generate
}

