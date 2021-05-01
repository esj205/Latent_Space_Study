#############1PL IRT
##packages and options
library(rstan)
# save a bare version of a compiled Stan program to the hard disk so that it does not need to be recompiled
rstan_options(auto_write=TRUE)
# execute multiple Markov chains in parallel
options(mc.cores = parallel::detectCores())

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

##fitting
fit_1pl <- stan(file = '1pl_IRT.stan', data = data1)

fit_1pl

#############Multilevel-2PL IRT
##data is same as above
##fitting
fit_2pl <- stan(file= '2pl_IRT.stan', data = data1)

fit_2pl

#Traceplot
traceplot(fit_2pl, pars = 'sigma_beta',inc_warmup = TRUE)
traceplot(fit_2pl, pars = 'sigma_gamma',inc_warmup = TRUE)
traceplot(fit_2pl, pars = 'mu_beta',inc_warmup = TRUE)

#Estimated Parameter
plot(fit_2pl, pars = 'mu_beta', show_density=TRUE)
plot(fit_2pl, pars = 'sigma_beta', show_density=TRUE)
plot(fit_2pl, pars = 'sigma_gamma', show_density=TRUE)
