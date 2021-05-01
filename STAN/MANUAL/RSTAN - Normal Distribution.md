# RSTAN - Normal Distribution

## Load the packages

```R
library(dplyr)
library(rstan)
```



## Model

$$
y \; \text{~} \; Normal(\mu, \sigma^2)
$$



## Data

도로별 일자별 통행속도 2020/08

출처: http://topis.seoul.go.kr/refRoom/openRefRoom_1.do

```R
data <- read.csv('./data/velocity.csv', header = T)
colnames(data)[13] <- 'speed'

#강남대로만 추출
kang <- data[data$도로명 == '강남대로',]
hist(kang$speed, breaks=20)

#list data
data1 <- list(N = nrow(kang), y = kang$speed)
```



## STAN Code

```c++
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
```



## Fitting

```R
fit1 <- stan(file = 'normal.stan', data = data1)
fit1
```



## Trace Plot

```R
traceplot(fit1, inc_warmup = TRUE)
```



## Estimated Parameter

```R
plot(fit1, pars = 'mu', show_density=TRUE)
plot(fit1, pars = 'sigma', show_density = TRUE)
```



## Extract Posterior Draws

```R
res <- extract(fit1)
```



## Save y_pred Posterior Draws

```R
y_pred <- res$y_pred
hist(y_pred, nclass=20)
```

