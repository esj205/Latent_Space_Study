data {
  int<lower=1> J; // number of students
  int<lower=1> K; // number of questions
  int<lower=1> N; // number of observations
  int<lower=1, upper=J> jj[N]; // student for observation n
  int<lower=1, upper=K> kk[N]; // question for observation n
  int<lower=0, upper=1> y[N]; // correctness for observation n
}


parameters {
  real alpha[J]; // ability of student j
  real beta[K]; // easiness of question k
  real sigma; // sigma prior for ability
  //real gamma;  
  vector[2] z[J]; // position of student j
  vector[2] w[K]; // position of question k
}


model {
  //Prior for z & w
  vector[2] zeros;
  matrix[2,2] identity;
  zeros = rep_vector(0, 2);
  identity = diag_matrix(rep_vector(1.0, 2));
  z ~ multi_normal(zeros, identity);
  w ~ multi_normal(zeros, identity);
  
  //prior for ability
  sigma ~ inv_gamma(1,1);
  alpha ~ normal(0, sigma); // prior for ability
  
  //prior for item
  beta ~ normal(0, 4);
  
  //prior for gamma
  //gamma ~ lognormal(0.5,1);
  
  //likelihood
  for(n in 1:N)
    y[n] ~ bernoulli_logit(alpha[jj[n]] + beta[kk[n]] - distance(z[jj[n]], w[kk[n]]));
}