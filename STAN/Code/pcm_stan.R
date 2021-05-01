library(rstan)
library(eRm)

pcm_data <- pcmdat
pcm_data

J = 20
K = 7

data1 <- list(
  J = J,
  K = K,
  N = J*K,
  jj = rep(1:J, times = K),
  kk = rep(1:K, each = J),
  y = as.numeric(unlist(pcm_data))
)

fit_pcm <- stan(file = 'pcm.stan', data = data1)
fit_pcm



##plot
plot(fit_pcm)


