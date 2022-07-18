rm(list=ls())
library(randomForest)
library(hdm)
source("4.2.1 Funs.R")

set.seed(20220630)

# Load data
load("4.1 NLS.RData")
X = X.mat[,c(1:4, 8,9,12, 14, 19,20,22)]


# data splitting
N=length(Z) 
K=2
inds.train.set = list()
inds.eval.set = list()
inds = 1:N
for (i in 1:K) {
  inds.eval.set[[i]] <- sample(inds,floor(length(inds)/(K-i+1)))
  inds.train.set[[i]] <- setdiff(1:N,inds.eval.set[[i]])
  inds = setdiff(inds,inds.eval.set[[i]])
}



# for loop


method_ps = c("rf") #j
method_mu = c("rf") #k

# for (j in 1) {
#   for (k in 1) {
    
    
    # initialize
    pi.hat  <- rep(0,N)
    pY1.hat <- rep(0,N)
    pY0.hat <- rep(0,N)
    pD1.hat <- rep(0,N)
    pD0.hat <- rep(0,N)
    
    # estimate nuisance parameters
    for (i in 1:K) {
      pi.hat[inds.eval.set[[i]]]<-estimate_ps(D,X,inds.train.set[[i]],inds.eval.set[[i]],method = method_ps[j])
      
      pY.hat<-estimate_mu(Y,Z,X,inds.train.set[[i]],inds.eval.set[[i]],method = method_mu[k]) 
      pY1.hat[inds.eval.set[[i]]]<-pY.hat[["mu1.hat.eval"]]
      pY0.hat[inds.eval.set[[i]]]<-pY.hat[["mu0.hat.eval"]]
      
      pD.hat<-estimate_mu(D,Z,X,inds.train.set[[i]],inds.eval.set[[i]],method = method_mu[k]) 
      pD1.hat[inds.eval.set[[i]]]<-pD.hat[["mu1.hat.eval"]]
      pD0.hat[inds.eval.set[[i]]]<-pD.hat[["mu0.hat.eval"]]
    }
    
    f.hat = Z*pi.hat + (1-Z)*pi.hat
    deltaD.hat = pD1.hat - pD0.hat
    deltaY.hat = pY1.hat - pY0.hat
    
    
    # Estimator
    MR_DML <- (2*Z-1)/f.hat/deltaD.hat*
      (Y- pY0.hat - (D-pD0.hat)*deltaY.hat/deltaD.hat ) +  deltaY.hat/deltaD.hat
    MR_DML = mean(MR_DML)
  
    
    
#   }
# }



