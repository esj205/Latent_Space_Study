raw <- read.csv('eda.csv')
raw <- raw[,-1]

#########################

library("Rcpp")
library(rstudioapi)
library(lsrm12pl)
library(ggplot2)

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

capture.output(output, file='output5.txt')

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



##Clustering
#item_clustering
item_x <- output$w_estimate[,1]
item_y <- output$w_estimate[,2]

item <- data.frame(x=item_x, y=item_y)
item.m <- as.matrix(item)

library(apcluster)
library(anocva)

sim <- expSimMat(item.m, method='euclidean')
cluster <- spectralClustering(sim, 5)
plot(as.matrix(item), col=cluster, pch=16, cex=1.3, xlim=c(-3,3))
text(item$x, item$y, pos=2, cex=.8)

#cluster group list
cl1 <- which(cluster==1)
cl2 <- which(cluster==2)
cl3 <- which(cluster==3)
cl4 <- which(cluster==4)
cl5 <- which(cluster==5)

colnames(raw)[cl1]
colnames(raw)[cl2]
colnames(raw)[cl3]
colnames(raw)[cl4]
colnames(raw)[cl5]

#mean coordinate for each cluster
mean_cl1 <- c(mean(item_x[cl1]),mean(item_y[cl1]))
mean_cl2 <- c(mean(item_x[cl2]),mean(item_y[cl2]))
mean_cl3 <- c(mean(item_x[cl3]),mean(item_y[cl3]))
mean_cl4 <- c(mean(item_x[cl4]),mean(item_y[cl4]))
mean_cl5 <- c(mean(item_x[cl5]),mean(item_y[cl5]))


cluster_mean <- rbind(mean_cl1, mean_cl2, mean_cl3, mean_cl4, mean_cl5)
cluster_mean
colnames(cluster_mean) <- c('w_x', 'w_y')
plot(cluster_mean, xlim=c(-3.5,3.5), ylim=c(-3.5,3.5), col=1:5, pch=16)
text(cluster_mean[,1], cluster_mean[,2],labels = rownames(cluster_mean) ,pos=1)

plot(output$z_estimate[,1]~output$z_estimate[,2],cex.axis=1,pch="",xlim=c(-3,3),ylim=c(-3,3))
points(output$z_estimate,pch=20,col=8)
points(cluster_mean, col=1:5, pch=8, cex=2)
text(cluster_mean[,1], cluster_mean[,2],labels = rownames(cluster_mean) ,pos=1)


#clustering_box
#cluster_boundary
cluster_boundary <- data.frame(x_low = cluster_mean[,1]-0.6, x_high = cluster_mean[,1]+0.6,
                               y_low = cluster_mean[,2]-0.6, y_high = cluster_mean[,2]+0.6)

#case clustering
case <- data.frame(case_x=output$z_estimate[,1], case_y=output$z_estimate[,2],
                   cluster=rep(0, nrow(output$z_estimate)))

for(i in 1:nrow(case)){
  for(j in 1:nrow(cluster_boundary))
  if(case$case_x[i]>cluster_boundary$x_low[j] &
     case$case_x[i]<cluster_boundary$x_high[j] &
     case$case_y[i]>cluster_boundary$y_low[j] &
     case$case_y[i]<cluster_boundary$y_high[j]){
    case$cluster[i]<-j
  }
}

case_clustering <- list(cluster1=which(case$cluster==1),
                        cluster2=which(case$cluster==2),
                        cluster3=which(case$cluster==3),
                        cluster4=which(case$cluster==4),
                        cluster5=which(case$cluster==5))

clustered_data <- case[case$cluster!=0,]
plot(clustered_data$case_x, clustered_data$case_y, col=clustered_data$cluster, pch=16)

#clustering_circle
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
text(cluster_mean[,1], cluster_mean[,2],labels = c('cl1','cl2','cl3','cl4','cl5') ,pos=1,
     cex=1)

case_clustering <- list(cluster1=which(case$cluster==1),
                        cluster2=which(case$cluster==2),
                        cluster3=which(case$cluster==3),
                        cluster4=which(case$cluster==4),
                        cluster5=which(case$cluster==5))

#clustered case 살펴보기
BQ <- read.csv('BQ.csv')
BQ <- BQ[,-1]
colnames(BQ) <- c('sex', 'age', 'edu','child','familyn','income','asset')
unique(BQ[,6])

for(i in 1:ncol(BQ)){
  BQ[,i]<-as.factor(BQ[,i])
}
str(BQ)
summary(BQ)


#Cluster별로 BQ데이터 나누기
BQ_cl1 <- BQ[case_clustering[[1]],]
BQ_cl2 <- BQ[case_clustering[[2]],]
BQ_cl3 <- BQ[case_clustering[[3]],]
BQ_cl4 <- BQ[case_clustering[[4]],]
BQ_cl5 <- BQ[case_clustering[[5]],]

summary(BQ_cl1)
summary(BQ_cl2)
summary(BQ_cl3)
summary(BQ_cl4)
summary(BQ_cl5)



library(foreign)
library(readstata13)
rawdata <- read.dta13('rawdata.dta')

write.csv(rawdata, file='rawdata_extenstion.csv')

BQ_extension <- read.csv(file='BQ_extension.csv')
BQ_extension <- BQ_extension[,-c(1,2)]
