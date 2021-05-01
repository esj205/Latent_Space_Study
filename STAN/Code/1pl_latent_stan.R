##data
set.seed(123)
library(ltm)
library(rstan)
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

fit_1pl <- stan(file = '1pl_latent.stan', data = data1)
fit_1pl

traceplot(fit_1pl, pars = 'w[2,2]')

extracted <- extract(fit_1pl)


w_list = matrix(NA, 8, 2)
for(i in 1:K){
  mean1 = mean(extracted$w[,i,1])
  mean2 = mean(extracted$w[,i,2])
  w_list[i,] = c(mean1, mean2)
}
plot(w_list)

z_list = matrix(NA, nrow = J, ncol = 2)
for(i in 1:J){
  mean1 = mean(extracted$z[,i,1])
  mean2 = mean(extracted$z[,i,2])
  z_list[i,] = c(mean1, mean2)
}

z_list
plot(z_list)

#Question: values of z and w are too small.