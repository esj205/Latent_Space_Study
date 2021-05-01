raw <- read.csv('eda.csv')
raw <- raw[,-1]

library("Rcpp")
library(rstudioapi)
library(lsrm12pl)
library(ggplot2)

cluster5 <- c("M2_1", "M2_2", "M2_3", "M2_4", "M2_5")

raw <- raw[,cluster5]

data     <- as.matrix(raw)
nsample  <- nrow(data)
nitem    <- ncol(data)
ndim     <- 2 
niter    <- 30000 #30000
nburn    <- 5000 #5000
nthin    <- 5
nprint   <- 500
jump_beta     <- 0.3
jump_theta    <- 1.0
jump_z        <- 0.5
# jump_theta    <- 0.5
# jump_z        <- 0.3
jump_w        <- 0.03
jump_gamma    <- 0.020

pr_mean_beta  <- 0
pr_sd_beta    <- 1
pr_mean_theta <- 0
pr_mean_gamma <- 0.5
pr_sd_gamma <- 1.0
prior_a <- 0.001
prior_b <- 0.001

output <- lsrm1pl_fixed_gamma(data,  ndim, niter, nburn, nthin, nprint,
                              jump_beta, jump_theta, jump_z, jump_w,pr_mean_beta, pr_sd_beta, pr_mean_theta, prior_a, prior_b)

output$accept_beta
output$accept_theta[1:50]
output$accept_z[1:50]
output$accept_w
output$accept_gamma

capture.output(output, file='cl5_fixed_output.txt')

## basic plot
pdf("cl5_trace_beta.pdf")
for(i in 1:ncol(output$beta)) ts.plot(output$beta[1:nrow(output$beta),i],main=paste("beta",i))
dev.off()

pdf("cl5_trace_theta.pdf")
for(i in 1:ncol(output$theta)) ts.plot(output$theta[,i],main=paste("theta",i))
dev.off()

pdf("cl5_trace_sigma_theta.pdf")
ts.plot(output$theta_sd,main="sigma_theta")
dev.off()


## lsrm plot
pdf("cl5_plot_w.pdf")
plot(output$w_estimate[,1]~output$w_estimate[,2],cex.axis=1.25,pch="",xlim=c(-3,3),ylim=c(-5,5))
points(output$w_estimate,pch=20,col=2)
text(output$w_estimate+0.1,labels=1:ncol(output$beta),cex=0.75,font=2,col=4)
dev.off()


pdf("cl5_plot_wz.pdf")
plot(output$z_estimate[,1]~output$z_estimate[,2],cex.axis=1,pch="",xlim=c(-3,3),ylim=c(-3,3))
points(output$z_estimate,pch=20,col=2)
text(output$w_estimate,labels=1:ncol(output$beta),cex=0.75,font=2,col=4)
dev.off()

library(ggplot2)
a <- as.data.frame(output$z_estimate)
colnames(a) <- c("coordinate_1","coordinate_2")
b <- as.data.frame(output$w_estimate)
colnames(b) <- c("coordinate_1","coordinate_2")
b$id <- seq(1,nrow(b)) 

ggplot() + 
  geom_text(data = b, aes(x=coordinate_1, y = coordinate_2, label = id),col=2) +
  geom_point(data = a,aes(x=coordinate_1,y=coordinate_2)) + 
  xlim (-2.5,2.5) + ylim(-2.5,2.5)


pdf("cl5_trace_z.pdf")
for(k in 1:nsample) ts.plot(output$z[,k,1],main=paste("z_1",k))
for(k in 1:nsample) ts.plot(output$z[,k,2],main=paste("z_2",k))
dev.off()

pdf("cl5_trace_w.pdf")
for(i in 1:nitem) ts.plot(output$w[,i,1],main=paste("w_1",i))
for(i in 1:nitem) ts.plot(output$w[,i,2],main=paste("w_2",i))
dev.off()

pdf("cl5_trace_gamma.pdf")
ts.plot(output$gamma,main="gamma")
dev.off()
