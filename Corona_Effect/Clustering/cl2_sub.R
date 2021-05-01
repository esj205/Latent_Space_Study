raw <- read.csv('eda.csv')
raw <- raw[,-1]

library("Rcpp")
library(rstudioapi)
library(lsrm12pl)
library(ggplot2)

cluster2 <- c("L1_1", "L1_2", "L1_3", "L1_4","L1_5", "L1_6", "L1_7", 
              "M1_1", "M1_2", "M1_3", "M1_4", "M1_5","M1_6", "M1_7", "M1_8", 
              "M1_9", "M1_10", "M1_11", "M1_12")

raw <- raw[,cluster2]

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
jump_w        <- 0.15
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

pdf("cl2_sub_trace_beta.pdf")
for(i in 1:ncol(output$beta)) ts.plot(output$beta[1:nrow(output$beta),i],main=paste("beta",i))
dev.off()

pdf("cl2_sub_trace_theta.pdf")
for(i in 1:ncol(output$theta)) ts.plot(output$theta[,i],main=paste("theta",i))
dev.off()

pdf("cl2_sub_trace_sigma_theta.pdf")
ts.plot(output$theta_sd,main="sigma_theta")
dev.off()


## lsrm plot
pdf("cl2_sub_plot_w.pdf")
plot(output$w_estimate[,1]~output$w_estimate[,2],cex.axis=1.25,pch="",xlim=c(-3,3),ylim=c(-5,5))
points(output$w_estimate,pch=20,col=2)
text(output$w_estimate+0.1,labels=1:ncol(output$beta),cex=0.75,font=2,col=4)
dev.off()


pdf("cl2_sub_plot_wz.pdf")
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


pdf("cl2_sub_trace_z.pdf")
for(k in 1:nsample) ts.plot(output$z[,k,1],main=paste("z_1",k))
for(k in 1:nsample) ts.plot(output$z[,k,2],main=paste("z_2",k))
dev.off()

pdf("cl2_sub_trace_w.pdf")
for(i in 1:nitem) ts.plot(output$w[,i,1],main=paste("w_1",i))
for(i in 1:nitem) ts.plot(output$w[,i,2],main=paste("w_2",i))
dev.off()

pdf("cl2_sub_trace_gamma.pdf")
ts.plot(output$gamma,main="gamma")
dev.off()

#clustering
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

cl1 <- which(cluster==1)
cl2 <- which(cluster==2)
cl3 <- which(cluster==3)

colnames(raw)[cl1]
colnames(raw)[cl2]
colnames(raw)[cl3]

mean_cl1 <- c(mean(item_x[cl1]),mean(item_y[cl1]))
mean_cl2 <- c(mean(item_x[cl2]),mean(item_y[cl2]))
mean_cl3 <- c(mean(item_x[cl3]),mean(item_y[cl3]))

cluster_mean <- rbind(mean_cl1, mean_cl2, mean_cl3)
cluster_mean
colnames(cluster_mean) <- c('w_x', 'w_y')
plot(cluster_mean, xlim=c(-3.5,3.5), ylim=c(-3.5,3.5), col=1:5, pch=16)
text(cluster_mean[,1], cluster_mean[,2],labels = rownames(cluster_mean) ,pos=1)

plot(output$z_estimate[,1]~output$z_estimate[,2],cex.axis=1,pch="",xlim=c(-3,3),ylim=c(-3,3))
points(output$z_estimate,pch=20,col=8)
points(cluster_mean, col=1:5, pch=8, cex=2)
text(cluster_mean[,1], cluster_mean[,2],labels = rownames(cluster_mean) ,pos=1)



library(TSdist)
case <- data.frame(case_x=output$z_estimate[,1], case_y=output$z_estimate[,2],
                   cluster=rep(0, nrow(output$z_estimate)))

for(i in 1:nrow(case)){
  for(j in 1:nrow(cluster_mean))
    if(EuclideanDistance(as.numeric(case[i,c(1,2)]), as.numeric(cluster_mean[j,]))<0.7){
      case$cluster[i]<-j
    }
}

clustered_data <- case[case$cluster!=0,]
plot(clustered_data$case_x, clustered_data$case_y, col=clustered_data$cluster, pch=16,
     xlim=c(-3,3), ylim=c(-3,3))
text(cluster_mean[,1], cluster_mean[,2],labels = c('cl1','cl2','cl3') ,pos=1,
     cex=1)

case_clustering <- list(cluster1=which(case$cluster==1),
                        cluster2=which(case$cluster==2),
                        cluster3=which(case$cluster==3))

case_clustering
capture.output(case_clustering, file='cl2_sub_case_clustering.txt')
