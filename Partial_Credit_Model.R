library(eRm)
library(readr)
library(plyr)
library(WrightMap)


#PCM with 10 subjects, 3 items


#PCM Modeling
res <- PCM(pcmdat)
res

summary(res)

res$hessian

#Plot
plotPImap(res, irug=TRUE)
plotICC(res)
plotICC(res, legpos=FALSE)


#Item Difficulty and Threshold

item.estimates <- thresholds(res)
item.estimates

item_difficulty <- item.estimates[["threshtable"]][["1"]]
item_difficulty

## Get threshold SEs values:
item.se <- item.estimates$se.thresh
item.se

#Person Locations(theta)
person.locations.estimate <- person.parameter(res)
summary(person.locations.estimate)

# Build a table for person locations
person_theta <- person.locations.estimate$theta.table
person_theta

#Item and person fit statistics
item.fit <- itemfit(person.locations.estimate)
item.fit

item.fit.table <- cbind(item.fit[["i.outfitMSQ"]],item.fit[["i.infitMSQ"]],item.fit[["i.infitMSQ"]],item.fit[["i.infitZ"]])
pfit <- personfit(person.locations.estimate)
pfit
person.fit.table <- cbind(pfit[["p.outfitMSQ"]],pfit[["p.outfitMSQ"]],pfit[["p.outfitMSQ"]],pfit[["p.outfitMSQ"]])
person.fit.table









N <- dim(pcmdat)[1]
mt_vek <- apply(pcmdat, 2, max, na.rm=TRUE)
mt_vek_0 <- mt_vek+1
X01_0 <- matrix(rep(0, (N*sum(mt_vek_0))), nrow=N) # number of persons * sum of number of categories 
K <- length(mt_vek)
cummt0 <- c(0, cumsum(mt_vek_0)[1:(K-1)])+1 #아이템 순서대로 카테고리를 일렬로 늘어놨을 때 0번째 카테고리 위치에 대한 index
indmatp <- apply(pcmdat, 1, function(xi){xi+cummt0})
imp1 <- as.vector(indmatp)
imp2 <- rep(1:N, rep(K,N))
indmat <- cbind(imp2, imp1)
X01_0[indmat] <- 1
NAindmat <- rbind(imp2,rep(1:K,N),c(t(pcmdat)))
rownames(NAindmat) <- NULL
NAind <- t(NAindmat[1:2,is.na(NAindmat[3,])])
if (length(NAind) > 0) {
  NAindlist <- apply(NAind,1,function(x){
    co <- seq(cummt0[x[2]],cummt0[x[2]]+mt_vek[x[2]])
    NAind01 <- cbind(rep(x[1],length(co)),co)
    data.frame(NAind01,row.names=NULL)                                               #list with NA indices
  })
  indmatNA <- matrix(unlist(lapply(NAindlist, function(x) {t(as.matrix(x))})),ncol=2,byrow=TRUE)   #matrix with NA indices 
  X01_0[indmatNA] <- NA
}
X01 <- X01_0[,-cummt0]

gmemb <- rep(1,dim(pcmdat)[1])
Groups <- 1 #그냥 PCM에서 Groups는 다 1
mpoints <- 1 #그냥 PCM에서 mpoints는 전부 1
levs <- (gmemb-1)*max(Groups)+Groups

x_mt <- colSums(X01,na.rm=TRUE)                #item category raw scores as vector
#NA그룹에서 x_mtlist를 분리
x_mtlist <- list(x_mt) 
ngroups <- 1


end1 <- length(mt_vek)*mpoints*ngroups
mt_ind <- rep(1:end1,rep(mt_vek,mpoints*ngroups)) #category index vector (for converting x_mt into list)
mt_ind
x_tmt <- split(x_mt,mt_ind)                       #list for likelihood: item-wise * ngroups
x_tmt #각 item에서 각 category가 몇 번 선택되었는지


rtot <- sum(mt_vek)*mpoints
rtot # 각 카테고리에서 가능한 가장 큰 number를 더했을 때 가능한 숫자.


ics <-  rep(sequence(mt_vek),mpoints)                 #item category scores for each item as vector
rv <- apply(X01,1,function(x) {                       #person raw scores of 0/1 matrix
  ics[!is.na(x)]%*%na.exclude(x)}) 
#그냥 사람별로 선택한 번호 다 더한 것


#--- preparing index vector for item parameters ---
gind <- rep(1,dim(X01)[1])



#--- preparing lists for person splits ---
rvlist <- split(rv,levs) #split person raw scores due to levels (NAgroup AND treatment)
nrlist <- lapply(rvlist,function(rvel) {    #list with item raw score frequencies for each group (transposed)
  rvtab <- table(rvel)                            #raw score frequencies
  dnamevek <- as.numeric(unlist(dimnames(rvtab))) #different raw scores for 0 fill up
  nr <- rep (0,rtot+1)                            #setting 0 raw score frequencies
  nr[dnamevek+1] <- rvtab #vector with person raw scores from 1:rtot (with 0 fill up)
  nr <- nr[-1]
  return(nr)
})

rvlist #응답자별로 선택한 번호들의 raw number를 다 더한 것
nrlist #rvlist를 오름차순으로 정렬했을 때 빈도를 나타내는 것


g_NA <- 1
gby <- gmemb




NAstruc <- by(!is.na(X01),gby,function(x) {                  #list of unique NA structures for each Group
  x.u <- unique(x)
  as.numeric(as.matrix(x.u))}) #NA's are coded with 0

NAcheck <- sapply(NAstruc,sum)                         #if for certain NAgroups only 1 item was presented

NAcheck




#################################################
##likelihood Function
#################################################
alpha = rep(NA, dim(pcmdat)[2]) #alpha list
theta = rep(NA, dim(pcmdat)[1]) #theta list
mt_vek <- apply(pcmdat, 2, max, na.rm=TRUE)
m = as.numeric(apply(pcmdat, 2, max, na.rm=TRUE))
beta = rep(NA, sum(m))
K <- length(mt_vek)
cummt0 <- c(0, cumsum(m)[1:(K-1)])+1
cummt0


#no alpha version
loglik_noalpha <- function(dat, beta, theta, m){
  #initial log-likelihood value
  logl = 0
  
  for(i in 1:dim(dat)[1]){
    #theta_i
    theta_i = theta[i]
    
    #initial log-likelihood value of ith respondent
    logl_i = 0
    
    for(j in 1:dim(dat)[2]){
      #initial beta index number
      start_cat = cummt0[j]
      
      #max category number
      m_j = m[j]
      
      #initial denominator and nominator
      denomi = 0
      nomi = 0
      
      
      #denominator value
      for(k in 0:m_j){
        value = 0
        for(h in 0:k){
          beta_jh = beta[start_cat+h]
          value = value + (theta_i-beta_jh)
        }
        denomi = denomi + exp(value)
      }
      
      
      #nominator value
      for(s in 0:dat[i,j]){
        beta_jh = beta[start_cat+s]
        nomi = nomi + (theta_i - beta_jh)
      }
      
      #log-likelihood of ith respondent and jth item
      logl_ij = nomi - log(denomi)
      
      #log-likelihood of ith respondent
      logl_i = logl_i + logl_ij
    }
    
    #sum of log-likelihood value of each respondent
    logl = logl + logl_i
    
  }
  return(logl)
}







#including alpha version
loglik <- function(dat, alpha, beta, theta, m){
  #initial log-likelihood value
  logl = 0
  
  for(i in 1:dim(dat)[1]){
    #theta_i
    theta_i = theta[i]
    
    #initial log-likelihood value of ith respondent
    logl_i = 0
    
    for(j in 1:dim(dat)[2]){
      alpha_j = alpha[j]
      start_cat = cummt0[j]
      m_j = m[j]
      denomi = 0
      nomi = 0
      
      
      #denominator value
      for(k in 0:m_j){
        value = 0
        for(h in 0:k){
          beta_jh = beta[start_cat+h]
          value = value + alpha_j*(theta_i-beta_jh)
        }
        denomi = denomi + exp(value)
      }
      
      
      #nominator value
      for(s in 0:dat[i,j]){
        beta_jh = beta[start_cat+s]
        nomi = nomi + alpha_j*(theta_i - beta_jh)
      }
      
      #log-likelihood of ith respondent and jth item
      logl_ij = nomi - log(denomi)
      
      #log-likelihood of ith respondent
      logl_i = logl_i + logl_ij
    }
    
    #sum of log-likelihood value of each respondent
    logl = logl + logl_i
    
  }
  return(logl)
}
