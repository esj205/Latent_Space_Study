raw <- read.csv('eda.csv')
raw <- raw[,-1]

library("Rcpp")
library(rstudioapi)
library(lsrm12pl)
library(ggplot2)

cluster3 <- c("J1_1", "J1_2", "J1_3", "J1_4", "J1_5", "J1_6", "J1_7")

raw <- raw[,cluster3]

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
jump_w        <- 0.07
jump_gamma    <- 0.025

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


## basic plot
pdf("cl3_trace_beta.pdf")
for(i in 1:ncol(output$beta)) ts.plot(output$beta[1:nrow(output$beta),i],main=paste("beta",i))
dev.off()

pdf("cl3_trace_theta.pdf")
for(i in 1:ncol(output$theta)) ts.plot(output$theta[,i],main=paste("theta",i))
dev.off()

pdf("cl3_trace_sigma_theta.pdf")
ts.plot(output$theta_sd,main="sigma_theta")
dev.off()


## lsrm plot
pdf("cl3_plot_w.pdf")
plot(output$w_estimate[,1]~output$w_estimate[,2],cex.axis=1.25,pch="",xlim=c(-5,5),ylim=c(-5,5))
points(output$w_estimate,pch=20,col=2)
text(output$w_estimate+0.1,labels=1:ncol(output$beta),cex=0.75,font=2,col=4)
dev.off()


pdf("cl3_plot_wz.pdf")
plot(output$z_estimate[,1]~output$z_estimate[,2],cex.axis=1,pch="",xlim=c(-5,5),ylim=c(-5,5))
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
  geom_point(data = a,aes(x=coordinate_1,y=coordinate_2))


pdf("cl3_trace_z.pdf")
for(k in 1:nsample) ts.plot(output$z[,k,1],main=paste("z_1",k))
for(k in 1:nsample) ts.plot(output$z[,k,2],main=paste("z_2",k))
dev.off()

pdf("cl3_trace_w.pdf")
for(i in 1:nitem) ts.plot(output$w[,i,1],main=paste("w_1",i))
for(i in 1:nitem) ts.plot(output$w[,i,2],main=paste("w_2",i))
dev.off()

pdf("cl3_trace_gamma.pdf")
ts.plot(output$gamma,main="gamma")
dev.off()

item_x <- output$w_estimate[,1]
item_y <- output$w_estimate[,2]

item <- data.frame(x=item_x, y=item_y)
item.m <- as.matrix(item)

library(apcluster)
library(anocva)

sim <- expSimMat(item.m, method='euclidean')
cluster <- spectralClustering(sim, 3)
plot(as.matrix(item), col=cluster, pch=16, cex=1.3, xlim=c(-3,3))
text(item$x, item$y, pos=2, cex=.8)


##respondent clustering

wzplot <-ggplot() + 
  geom_text(data = b, aes(x=coordinate_1, y = coordinate_2, label = id),col=2) +
  geom_point(data = a,aes(x=coordinate_1,y=coordinate_2)) 

wzplot + geom_abline(intercept = -0.35, slope = -1.1)

list1 <- c()
for(i in 1:1000){
  if(a$coordinate_2[i] < -0.35 - 1.1*a$coordinate_1[i]){
    list1 <- c(list1, i)
  }
}

summary(raw)
list1_raw <- raw[list1,]
summary(list1_raw)


##BQ
BQ <- read.csv(file = 'BQ_extension.csv')
BQ$BQ1 <- as.factor(BQ$BQ1)
BQ$BQ2 <- as.factor(BQ$BQ2)
BQ$BQ3_n2 <- as.factor(BQ$BQ3_n2)
BQ$BQ4 <- as.factor(BQ$BQ4)
BQ$BQ8 <- as.factor(BQ$BQ8)
BQ$BQ5 <- as.factor(BQ$BQ5)
BQ$BQ5_1 <- as.factor(BQ$BQ5_1)
BQ$BQ5_2 <- as.factor(BQ$BQ5_2)
BQ$BQ5_3 <- as.factor(BQ$BQ5_3)
BQ$BQ6 <- as.factor(BQ$BQ6)
BQ$BQ7 <- as.factor(BQ$BQ7)
BQ$BQ7_1 <- as.factor(BQ$BQ7_1)
BQ$BQ7_2 <- as.factor(BQ$BQ7_2)
BQ$BQ9 <- as.factor(BQ$BQ9)
BQ$BQ12_1 <- as.factor(BQ$BQ12_1)
BQ$BQ12_2 <- as.factor(BQ$BQ12_2)
BQ$BQ12_3 <- as.factor(BQ$BQ12_3)
BQ$BQ13 <- as.factor(BQ$BQ13)
BQ$BQ14 <- as.factor(BQ$BQ14)
BQ$BQ14_1 <- as.factor(BQ$BQ14_1)



list1_BQ <- BQ[list1,]
summary(BQ)
summary(list1_BQ)
