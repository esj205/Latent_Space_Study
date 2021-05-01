###########Logistic Regression
#####Bernoulli-Logistic
library(rstan)
library(survival)

##data
str(colon)
colon <- na.omit(colon)
colon[,-1]
selected_cols <- c('status', 'sex', 'age', 'obstruct', 
  'perfor', 'adhere', 'nodes', 'differ', 'extent', 'surg')
colon <- colon[,selected_cols]

str(colon)
colon$differ <- as.ordered(colon$differ)
colon$extent <- as.ordered(colon$extent)


design_matrix <- model.matrix(status ~., data = colon)


##list data
data1 <- list(
  N = nrow(design_matrix),
  x = design_matrix[,-1],
  K = ncol(design_matrix) - 1,
  y = colon$status
)


#fitting
fit1 <- stan(file = 'logistic_reg.stan', data = data1)

##traceplot
traceplot(fit1, pars = 'alpha',inc_warmup = TRUE)
traceplot(fit1, pars = 'beta[1]',inc_warmup = TRUE)

#Estimated Parameters
plot(fit1, pars = 'alpha', show_density=TRUE)
plot(fit1, pars = 'beta[1]', show_density = TRUE)
plot(fit1, pars = 'beta[2]', show_density = TRUE)


#####Multi-logit
##data
library(foreign)
ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

mldata <- ml[,c('prog', 'ses', 'write')]
str(mldata)

mldata$ses <- as.ordered(mldata$ses)
str(mldata)

mldata$prog <- as.numeric(mldata$prog)
design_matrix2 <- model.matrix(prog ~ ., data = mldata)



##list data
data2 <- list(
  K = length(unique(mldata$prog)),
  N = nrow(design_matrix2),
  D = ncol(design_matrix2),
  y = mldata$prog,
  x = design_matrix2
)

##fitting
fit2 <- stan(file = 'multi_logit.stan', data = data2)
fit2

##traceplot
traceplot(fit2)

#Estimated Parameters
plot(fit2, pars = 'beta[1,1]', show_density=TRUE)
plot(fit2, pars = 'beta[1,2]', show_density = TRUE)
plot(fit2, pars = 'beta[4,3]', show_density = TRUE)

##summary
summary(fit2, par = 'beta', prob = 0.5)$summary
