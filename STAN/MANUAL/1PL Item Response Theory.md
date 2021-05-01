# 1PL Item Response Theory

## Load the Packages

```R
library(rstan)
```



## Model

$$
P_{kj} = \frac{e^{\alpha_j - \beta_k + \delta}}{1+e^{\alpha_j - \beta_k + \delta}}
$$

$$
P_{kj} = \text{probability of success by person j on item k}
$$

$$
\alpha_j:\text{ability of person j}
$$

$$
\beta_k : \text{difficulty of item k}
$$

$$
\delta:\text{mean person ability}
$$

## Data

ltm library 안에 있는 Mobility 데이터 사용.

```R
##data
set.seed(123)
library(ltm)
response <- Mobility[sample(1:nrow(Mobility), 20, replace =FALSE),]
head(response)


##list data
J = 20
K = 8

data1 <- list(
  J = J,
  K = K,
  N = J*K,
  jj = rep(1:J, times = K),
  kk = rep(1:K, each = J),
  y = as.numeric(unlist(response))
)
```



## STAN Code

```c++
data {
  int<lower=1> J; // number of students
  int<lower=1> K; // number of questions
  int<lower=1> N; // number of observations
  int<lower=1, upper=J> jj[N]; // student for observation n
  int<lower=1, upper=K> kk[N]; // question for observation n
  int<lower=0, upper=1> y[N]; // correctness for observation n
}


parameters {
  real delta; // mean student ability
  real alpha[J]; // ability of student j - mean ability
  real beta[K]; // difficulty of question k
}


model {
  alpha ~ std_normal(); // informative true prior
  beta ~ std_normal(); // informative true prior
  delta ~ normal(0.75, 1); // informative true prior
  for(n in 1:N)
    y[n] ~ bernoulli_logit(alpha[jj[n]] - beta[kk[n]] + delta);
}
```



## Fitting

```R
fit_1pl <- stan(file = '1pl_IRT.stan', data = data1)
fit_1pl
```
