# RSTAN - Poisson Regression

## Model(Including Offset)

$$
Y \; \text{~} \; Poisson(\mu)
$$

$$
log(\hat\mu) = X*\beta + \text{offset}
$$

## Data

ISwR package 안에 있는 eba1977 데이터 사용.

```R
#data
library(ISwR)
data("eba1977")

## Model matrix
design_matrix = model.matrix(cases ~ age + city + offset(log(pop)), data = eba1977)
modMat <- as.data.frame(design_matrix)
modMat$offset <- log(eba1977$pop)
names(modMat) <- c("intercept", "age55_59", "age60_64", "age65_69", "age70_74", 
                   "age75plus", "cityHorsens", "cityKolding", "cityVejle", "offset")

#list data
dat   <- as.list(modMat)
dat$y <- eba1977$cases
dat$N <- nrow(modMat)
dat$p <- ncol(modMat) - 1
```



## STAN Code

```c++
data {
int<lower=0> N; // Number of observations
int<lower=0> p; // Number of beta parameters
// Covariates
int <lower=0, upper=1> intercept[N];
int <lower=0, upper=1> age55_59[N];
int <lower=0, upper=1> age60_64[N];
int <lower=0, upper=1> age65_69[N];
int <lower=0, upper=1> age70_74[N];
int <lower=0, upper=1> age75plus[N];
int <lower=0, upper=1> cityHorsens[N];
int <lower=0, upper=1> cityKolding[N];
int <lower=0, upper=1> cityVejle[N];

real offset[N]; //offset
int<lower=0> y[N]; //outcomes
}

parameters {
real beta[p];
}


transformed parameters{
  real lp[N]; 
  real <lower=0> mu[N];
  
  for(i in 1:N){
    //Linear Predictor
    lp[i] = beta[1] + beta[2]*age55_59[i] + beta[3]*age60_64[i] + beta[4]*age65_69[i] + beta[5]*age70_74[i] + beta[6]*age75plus[i]+ beta[7]*cityHorsens[i] + beta[8]*cityKolding[i] + beta[9]*cityVejle[i] + offset[i];
    
    //Mean
    mu[i] = exp(lp[i]);
  }
}

model{
  y ~ poisson(mu);
}
```



## Fitting

```R
fit1 <- stan(file = 'poisson_code.stan', data = dat)
```



## Trace Plot

```R
traceplot(fit1, inc_warmup = TRUE)
```



## Estimated Parameters

```R
plot(fit1, show_density = TRUE)
```

