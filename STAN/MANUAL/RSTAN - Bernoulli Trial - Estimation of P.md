# RSTAN - Bernoulli Trial - Estimation of P

## Load the packages

```R
library(rstan)
```



## Model

$$
y \; \text{~} \; Bernoulli(p)  
$$

$$
p \; \text{~} \; beta(1,1)
$$



## Data

10번 시행, 7번 성공

```
N <- 10
y <- c(1,1,0,0,1,0,1,1,1,1)
data1 <- list(N=N, y=y)
```



## STAN Code

```
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
```



## Fitting

```R
fit1 <- stan(file = 'bernoulli.stan', data=data1, seed=123)
fit1
```



## Plot

```R
traceplot(fit1, inc_warmup = TRUE)
plot(fit1)
plot(fit1, show_density = TRUE)
```

