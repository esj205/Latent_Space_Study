###########Linear Regression
library(rstan)


#####Simple Linear Regression

#data
data1 <- cars

#list_data
data1 <- list(
  N = nrow(data1),
  y = data1$dist,
  x = data1$speed
)

#scatter plot
plot(data1$x, data1$y, xlab = 'speed', ylab='dist')


#fitting
fit1 <- stan(file = 'simple_linear.stan', data = data1)


#traceplot
traceplot(fit1, inc_warmup = TRUE)

#Estimated Parameters
plot(fit1, pars = 'alpha', show_density=TRUE)
plot(fit1, pars = 'beta', show_density=TRUE)
plot(fit1, pars = 'sigma', show_density=TRUE)

#Extract posterior draws
res <- extract(fit1)


#####Multiple Linear Regression
##No categorical data
#data
data2 <- attitude

#listdata
data2 <- list(
  N = nrow(data2),
  x = data2[,-1],
  K = ncol(data2)-1,
  y = data2$rating
)

#fitting
fit2 <- stan(file = 'multiple_linear.stan', data = data2)

#traceplot
traceplot(fit2, inc_warmup = TRUE)

#Estimated Parameters
plot(fit2, pars = 'alpha', show_density=TRUE)
plot(fit2, pars = 'beta[1]', show_density = TRUE)
plot(fit2, pars = 'beta[2]', show_density = TRUE)
plot(fit2, pars = 'sigma', show_density = TRUE)


##Categorical data included
#data
library(MASS)
str(birthwt)
birthwt$race <- as.factor(birthwt$race)

str(birthwt)
design_matrix <- model.matrix(bwt ~ ., data = birthwt)

#listdata
data3 <- list(
  N = nrow(design_matrix),
  x = design_matrix[,-1],
  K = ncol(design_matrix) - 1,
  y = birthwt$bwt
)

#fitting
fit3 <- stan(file = 'multiple_linear.stan', data = data3)

#traceplot
traceplot(fit3, inc_warmup = TRUE)

#Estimated Parameters
plot(fit3, pars = 'alpha', show_density = TRUE)
plot(fit3, pars = 'beta[1]', show_density = TRUE)
plot(fit3, pars = 'sigma', show_density=TRUE)

