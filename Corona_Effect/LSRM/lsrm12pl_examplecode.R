rm(list=ls())
library("Rcpp")
## Set Current Working Path
# if (!is.element("rstudioapi",installed.packages()[,1])){
#   install.packages("rstudioapi")
# }

library(rstudioapi)
current_path <-rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
library(lsrm12pl)
#read data

data     <- read.csv("drv.txt",sep=" ",header = F)
data     <- as.matrix(data)
nsample  <- nrow(data)
nitem    <- ncol(data)
ndim     <- 2 
niter    <- 15000
nburn    <- 2500
nthin    <- 5
nprint   <- 500
jump_beta     <- 0.4
jump_theta    <- 1.0
jump_z        <- 0.5
# jump_theta    <- 0.5
# jump_z        <- 0.3
jump_w        <- 0.25
jump_gamma    <- 0.025
jump_delta    <-1

pr_mean_beta  <- 0
pr_sd_beta    <- 1
pr_mean_theta <- 0
pr_sd_theta   <- 1
pr_mean_gamma <- 0.5
pr_sd_gamma <- 1.0
prior_a <- 0.001
prior_b <- 0.001

#twopl
jump_alpha   <-1.5 #two pl
#jump_alpha   <-0.025 #twopl_intrm 
jump_alpha   <-1.0 #two pl
pr_mean_alpha <- 0.5
pr_sd_alpha   <- 1

#intrm
pr_mean_delta<-0 
pr_sd_delta <-1

## onepl, twopl
output <- lsrm12pl::onepl(data, niter, nburn, nthin, nprint,
                   jump_beta, jump_theta, pr_mean_beta, pr_sd_beta, 
                   pr_mean_theta, prior_a, prior_b)

output <- lsrm12pl::twopl(data,niter,nburn,nthin,nprint,jump_beta,jump_theta,
                           jump_alpha,pr_mean_beta,pr_sd_beta,pr_mean_theta,prior_a,prior_b,
                           pr_mean_alpha,pr_sd_alpha)


## lsrm onepl, lsrm twopl
output <- lsrm12pl::lsrm1pl(data,  ndim, niter, nburn, nthin, nprint,
                            jump_beta, jump_theta, jump_gamma, jump_z,jump_w,
                            pr_mean_beta, pr_sd_beta, pr_mean_theta, pr_mean_gamma, pr_sd_gamma, 
                            prior_a, prior_b)

output <- lsrm12pl::lsrm2pl(data,  ndim, niter,nburn,nthin,nprint,
                  jump_beta, jump_theta, jump_alpha, jump_gamma, jump_z, jump_w,
                  pr_mean_beta, pr_sd_beta, prior_a,prior_b, pr_mean_theta,
                  pr_mean_gamma,pr_sd_gamma, pr_mean_alpha,  pr_sd_alpha)


## lsrm onepl fixed gamma=1,lsrm twopl fixed gamma=1
output <- lsrm12pl::lsrm1pl_fixed_gamma(data,  ndim, niter, nburn, nthin, nprint,
                     jump_beta, jump_theta, jump_z, jump_w,pr_mean_beta, pr_sd_beta, pr_mean_theta, prior_a, prior_b)

output <- lsrm12pl::lsrm2pl_fixed_gamma(data,  ndim, niter, nburn, nthin, nprint,
                                        jump_beta, jump_theta, jump_alpha, jump_z,jump_w,
                                        pr_mean_beta, pr_sd_beta, prior_a, prior_b, pr_mean_theta, pr_mean_alpha, pr_sd_alpha)

## onepl interaction, twopl interaction
output <- lsrm12pl::intrm1pl(data,  
                              niter, nburn, nthin, nprint, 
                              jump_beta, jump_theta, jump_delta, 
                              pr_mean_beta, pr_sd_beta, 
                              prior_a, prior_b, pr_mean_theta,
                              pr_mean_delta, pr_sd_delta)

output <- lsrm12pl::intrm2pl(data,  niter, nburn, nthin, nprint,
                              jump_beta, jump_theta,jump_alpha,jump_delta,
                              pr_mean_beta, pr_sd_beta, 
                              prior_a,  prior_b, pr_mean_theta,
                              pr_mean_delta, pr_sd_delta, pr_mean_alpha, pr_sd_alpha)

## lsrm2pl normal version

data     <- as.matrix( read.csv("24Factors_Trimmed_scaled_regression_5.csv"))
pr_a_sigma = 0.001
pr_b_sigma= 0.001
pr_a_th_sigma = 0.001
pr_b_th_sigma = 0.001


output <- lsrm1pl_normal(data, 
  ndim, niter, nburn, nthin, nprint,
  jump_beta, jump_theta, jump_gamma, jump_z, jump_w,
  pr_mean_beta, pr_sd_beta, pr_a_th_sigma, pr_b_th_sigma, pr_mean_theta,
  pr_a_sigma, pr_b_sigma, pr_mean_gamma, pr_sd_gamma)

output <- lsrm12pl::lsrm2pl_normal(data, 
                          ndim, niter, nburn, nthin, nprint,
                          jump_beta, jump_theta, jump_alpha, jump_gamma, jump_z, jump_w,
                          pr_mean_beta, pr_sd_beta, pr_a_th_sigma, pr_b_th_sigma, pr_mean_theta,
                          pr_a_sigma, pr_b_sigma, pr_mean_gamma, pr_sd_gamma, pr_mean_alpha, pr_sd_alpha)

## onepl interaction, twopl interaction normal version
pr_a_sigma = 0.001
pr_b_sigma= 0.001
pr_a_th_sigma = 0.001
pr_b_th_sigma = 0.001

output <- lsrm12pl::intrm1pl_normal(data, niter, nburn, nthin, nprint,
                                    jump_beta, jump_theta,jump_delta,
                                    pr_mean_beta, pr_sd_beta, 
                                    pr_a_sigma,  pr_b_sigma,pr_a_th_sigma,pr_b_th_sigma, pr_mean_theta,
                                    pr_mean_delta, pr_sd_delta)

output <- lsrm12pl::intrm2pl_normal(data, niter, nburn, nthin, nprint,
                                    jump_beta, jump_theta,jump_alpha,jump_delta,
                                    pr_mean_beta, pr_sd_beta, 
                                    pr_a_sigma,  pr_b_sigma,pr_a_th_sigma,pr_b_th_sigma, pr_mean_theta,
                                    pr_mean_delta, pr_sd_delta)


t(output$accept_beta)
t(output$accept_theta)
t(output$accept_w)
t(output$accept_z)
t(output$accept_alpha)
output$accept_gamma

## basic plot
pdf("trace_beta.pdf")
for(i in 1:ncol(output$beta)) ts.plot(output$beta[1:nrow(output$beta),i],main=paste("beta",i))
dev.off()

pdf("trace_theta.pdf")
for(i in 1:ncol(output$theta)) ts.plot(output$theta[,i],main=paste("theta",i))
dev.off()

pdf("trace_sigma_theta.pdf")
ts.plot(output$theta_sd,main="sigma_theta")
dev.off()

# twopl(basic plot)
pdf("trace_alpha.pdf")
for(i in 1:ncol(output$alpha)) ts.plot(log(output$alpha[1:nrow(output$alpha),i]),main=paste("alpha",i))
dev.off()

## lsrm plot
pdf("plot_w.pdf")
plot(output$w_estimate[,1]~output$w_estimate[,2],cex.axis=1.25,pch="",xlim=c(-3,3),ylim=c(-5,5))
points(output$w_estimate,pch=20,col=2)
text(output$w_estimate+0.1,labels=1:ncol(output$beta),cex=0.75,font=2,col=4)
dev.off()


pdf("plot_wz.pdf")
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


pdf("trace_z.pdf")
for(k in 1:nsample) ts.plot(output$z[,k,1],main=paste("z_1",k))
for(k in 1:nsample) ts.plot(output$z[,k,2],main=paste("z_2",k))
dev.off()

pdf("trace_w.pdf")
for(i in 1:nitem) ts.plot(output$w[,i,1],main=paste("w_1",i))
for(i in 1:nitem) ts.plot(output$w[,i,2],main=paste("w_2",i))
dev.off()

pdf("trace_gamma.pdf")
ts.plot(output$gamma,main="gamma")
dev.off()

## intrm plot 
pdf("ls_item_trace.pdf")
for(i in 1:nitem){
  for(j in 1:ndim) ts.plot(output$ls_item[,i,j],main=paste("item",i,"-",j))
}
dev.off()

pdf("ls_sample_trace.pdf")
for(i in 1:nsample){
  for(j in 1:ndim) ts.plot(output$ls_sample[,i,j],main=paste("sample",i,"-",j))
}
dev.off()

pdf("ls_lambda_trace.pdf")
for(j in 1:ndim) ts.plot(output$ls_lambda[,j],main=paste("lambda",j))
dev.off()

pdf("ls_plot.pdf")
temp = rbind(output$ls_mean_item,output$ls_mean_sample)
plot(output$ls_mean_item[,1]~output$ls_mean_item[,2],cex.axis=1.25,pch="",xlim=c(min(temp[,1]-0.1),max(temp[,1]+0.1)),ylim=c(min(temp[,2]-0.1),max(temp[,2]+0.1)))
#points(output$ls_mean_item,pch=20,col=2)
points(output$ls_mean_sample,pch=20,col=2)
text(output$ls_mean_item,labels=1:ncol(output$beta),cex=0.75,font=2,col=4)
dev.off()

## plot of normal version
library(ggplot2)

z <- data.frame(
  id = 1:nsample,
  coordinate1 = output$z_estimate[,1],
  coordinate2 = output$z_estimate[,2]
)
w <- data.frame(
  id = colnames(data),
  coordinate1 = output$w_estimate[,1],
  coordinate2 = output$w_estimate[,2]
)

ggplot() + 
  geom_point(data = z, aes(x = coordinate1, y = coordinate2)) +
  geom_text(data = w, aes(x = coordinate1, y = coordinate2, label=id), color = "red")


