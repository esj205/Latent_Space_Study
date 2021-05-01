raw <- read.csv('eda.csv')
raw <- raw[,-1]

library("Rcpp")
library(rstudioapi)
library(lsrm12pl)
library(ggplot2)

cluster4 <- c("J2_1", "J2_2", "J2_3", "J2_4", "J2_5", "J2_6", "J2_7", "J2_8",
              "J2_9", "J2_10")

raw <- raw[,cluster4]

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
jump_w        <- 0.12
jump_gamma    <- 0.025

pr_mean_beta  <- 0
pr_sd_beta    <- 1
pr_mean_theta <- 0
pr_mean_gamma <- 0.5
pr_sd_gamma <- 1.0
prior_a <- 0.001
prior_b <- 0.001

output <- lsrm12pl::lsrm1pl(data,  ndim, niter, nburn, nthin, nprint,
                            jump_beta, jump_theta, jump_gamma, jump_z,jump_w,
                            pr_mean_beta, pr_sd_beta, pr_mean_theta, pr_mean_gamma, pr_sd_gamma, 
                            prior_a, prior_b)
output$accept_beta
output$accept_theta[1:50]
output$accept_z[1:50]
output$accept_w
output$accept_gamma

capture.output(output, file='cl4_output.txt')

## basic plot
pdf("cl4_trace_beta.pdf")
for(i in 1:ncol(output$beta)) ts.plot(output$beta[1:nrow(output$beta),i],main=paste("beta",i))
dev.off()

pdf("cl4_trace_theta.pdf")
for(i in 1:ncol(output$theta)) ts.plot(output$theta[,i],main=paste("theta",i))
dev.off()

pdf("cl4_trace_sigma_theta.pdf")
ts.plot(output$theta_sd,main="sigma_theta")
dev.off()


## lsrm plot
pdf("cl4_plot_w.pdf")
plot(output$w_estimate[,1]~output$w_estimate[,2],cex.axis=1.25,pch="",xlim=c(-3,3),ylim=c(-5,5))
points(output$w_estimate,pch=20,col=2)
text(output$w_estimate+0.1,labels=1:ncol(output$beta),cex=0.75,font=2,col=4)
dev.off()


pdf("cl4_plot_wz.pdf")
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


pdf("cl4_trace_z.pdf")
for(k in 1:nsample) ts.plot(output$z[,k,1],main=paste("z_1",k))
for(k in 1:nsample) ts.plot(output$z[,k,2],main=paste("z_2",k))
dev.off()

pdf("cl4_trace_w.pdf")
for(i in 1:nitem) ts.plot(output$w[,i,1],main=paste("w_1",i))
for(i in 1:nitem) ts.plot(output$w[,i,2],main=paste("w_2",i))
dev.off()

pdf("cl4_trace_gamma.pdf")
ts.plot(output$gamma,main="gamma")
dev.off()