library(rstan)
##############1. Bernoulli Trial - estimation of p
N <- 10
y <- c(1,1,0,0,1,0,1,1,1,1)
data1 <- list(N=N, y=y)
fit1 <- stan(file = 'bernoulli.stan', data=data1, seed=123)
fit1
summary(fit1)$summary

#defining stan code in the script
bernoulli_code <- '
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
'

fit1 <- stan(model_code = bernoulli_code, data=data1, seed=123)
fit1


##MCMC warmup
traceplot(fit1, inc_warmup = TRUE)
plot(fit1)
plot(fit1, show_density = TRUE)

##Now, we have 100 observations
N <- 100
y <- c(rep(0,30), rep(1,70))
data2 <- list(N=N, y=y)
model <- stan_model('fit.stan')
fit2 <- sampling(model, data=data2, seed=123)

fit2
plot(fit2, show_density=TRUE, ci_level=0.8, xlim=c(0,1))


##베이즈 추정량

##extract() to get MCMC samples
res1 <- extract(fit1)
res2 <- extract(fit2)

## posterior mean
mean(res1$p)
mean(res2$p)

##MAP
optimizing(model, data=data1)

###plotting
#using bayesplot package
#install.packages('bayesplot')
library(bayesplot)

mcmc_areas(
  fit1,
  pars = c('p'),
  prob = 0.8, # 80% credible interval
  prob_outer = 0.99, #99% prob
  point_est = 'mean'
)

mcmc_areas(
  fit2,
  pars = c('p'),
  prob = 0.8, # 80% credible interval
  prob_outer = 0.99, #99% prob
  point_est = 'mean'
)

#histogram for mcmc samples
mcmc_hist(fit1)

#trace plot for mcmc samples
mcmc_trace(fit1, facet_args = list(nrow=2))
