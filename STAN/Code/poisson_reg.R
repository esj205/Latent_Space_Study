library(rstan)

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

#Fitting
fit1 <- stan(file = 'poisson_code.stan', data = dat)

#Trace plot
traceplot(fit1, inc_warmup = TRUE)

#Estimated Parameters
plot(fit1, show_density = TRUE)

