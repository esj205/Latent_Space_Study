#############Normal Distribution

library(dplyr)
library(rstan)

##Import Data
#data 출처
#http://topis.seoul.go.kr/refRoom/openRefRoom_1.do
#도로별 일자별 통행속도 2020/08
data <- read.csv('./data/velocity.csv', header = T)
colnames(data)[13] <- 'speed'

#강남대로만 추출
kang <- data[data$도로명 == '강남대로',]
hist(kang$speed, breaks=20)

#list data
data1 <- list(N = nrow(kang), y = kang$speed)

#fitting
fit1 <- stan(file = 'normal.stan', data = data1)

fit1

#Traceplot
traceplot(fit1, inc_warmup = TRUE)

#Estimated Parameter
plot(fit1, pars = 'mu', show_density=TRUE)

#Extract Posterior draws
res <- extract(fit1)

#save y_pred posterior draws
y_pred <- res$y_pred
hist(y_pred, nclass=20)

#using 'bayesplot' package
library(bayesplot)

#pred hist
mcmc_hist(fit1, 'y_pred')

#estimated posterior density curves
mcmc_areas(fit1, 'y_pred', prob=0.95) +
  ggtitle('Posterior distibution of y_pred', 'with median and 95% interval')

#posterior density of mu
mcmc_areas(fit1, 'mu', prob=0.8)
quantile(res$mu, c(0.1, 0.9))
