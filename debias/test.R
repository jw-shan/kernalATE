rm(list = ls())
library(randomForest)
library(ggplot2)
library(grid)
source("Datagen.R")
source("estimator_new.R")

# generate data
SEED <- 2
n <- 1000
p <- 500
p1 <- 4

data1 <- Datagen(SEED,n,p,p1)

Y  = data1$Y
D  = data1$D
X  = data1$X
Z  = data1$Z


# # test - estimation of nuisance
# ##y
# rf.data <- data.frame(Y=Y,Z=Z,X=X)
# rf.reg <- randomForest(Y~.,rf.data)
# yhat <- predict(rf.reg)
# 
# res <- data.frame(y1=data1$Y.1.true,yhat=yhat)
# plt1 <- ggplot(data=res[Z==1,],aes(y1,yhat)) +geom_point(size=0.7)+geom_abline(slope=1,color="red" )+
#   labs(x="y",y="y.hat",title = paste("Y(1),DGP1,RF,train,n=",as.character(n),",p=",as.character(p),sep = ""))
# plt2 <- ggplot(data=data.frame(num=1:sum(Z==0),yhat=res[Z==0,2]),aes(num,yhat)) +geom_point(size=0.7)+
#   geom_abline(slope=0,color="red" )+expand_limits(y=-0.1)+
#   labs(x="y",y="y.hat",title = paste("Y(0),DGP1,RF,train,n=",as.character(n),",p=",as.character(p),sep = ""))
# 
# ## D
# rf.data <- data.frame(D=D,Z=Z,X=X)
# rf.reg <- randomForest(D~.,rf.data)
# Dhat <- predict(rf.reg)
# 
# res <- data.frame(D1=data1$D.1.true,Dhat=Dhat)
# plt1 <- ggplot(data=res[Z==1,],aes(D1,Dhat)) +geom_point(size=0.7)+geom_abline(slope=1,color="red" )+
#   labs(x="D",y="D.hat",title = paste("D(1),DGP1,RF,train,n=",as.character(n),",p=",as.character(p),sep = ""))
# plt2 <- ggplot(data=data.frame(num=1:sum(Z==0),Dhat=res[Z==0,2]),aes(num,Dhat)) +geom_point(size=0.7)+
#   geom_abline(slope=0,intercept=0.2,color="red" )+expand_limits(y=-0.1)+
#   labs(x="D",y="D.hat",title = paste("D(0),DGP1,RF,train,n=",as.character(n),",p=",as.character(p),sep = ""))
# 
# ## ps
# rf.data <- data.frame(Z=Z,X=X)
# rf.reg <- randomForest(Z~.,rf.data)
# ps.hat <- predict(rf.reg)
# 
# res <- data.frame(ps=data1$pix.true,ps.hat=ps.hat)
# plt1 <- ggplot(data=res,aes(ps,ps.hat)) +geom_point(size=0.7)+geom_abline(slope=1,color="red" )+
#   labs(x="ps",y="ps.hat",title = paste("ps,DGP1,RF,train,n=",as.character(n),",p=",as.character(p),sep = ""))



# data splitting
K=2
inds.train.set = list()
inds.eval.set = list()
inds = 1:n
for (i in 1:K) {
  inds.eval.set[[i]] <- sample(inds,floor(length(inds)/(K-i+1)))
  inds.train.set[[i]] <- setdiff(1:n,inds.eval.set[[i]])
  inds = setdiff(inds,inds.eval.set[[i]])
}


# estimate
### initial
pY1.hat <- rep(0,n)
pY0.hat <- rep(0,n)
pD1.hat <- rep(0,n)
pD0.hat <- rep(0,n)
ps.hat <- rep(0,n)

for (i in 1:K) {
  ps.hat[inds.eval.set[[i]]]<-estimate_ps(Z,X,inds.train.set[[i]],inds.eval.set[[i]])
  
  pY.hat<-estimate_reg(Y,Z,X,inds.train.set[[i]],inds.eval.set[[i]])
  ### Extract regression function
  pY1.hat[inds.eval.set[[i]]]<-pY.hat[["mu1.hat.eval"]]
  pY0.hat[inds.eval.set[[i]]]<-pY.hat[["mu0.hat.eval"]]

  pD.hat<-estimate_reg(D,Z,X,inds.train.set[[i]],inds.eval.set[[i]])
  ### Extract regression function
  pD1.hat[inds.eval.set[[i]]]<-pD.hat[["mu1.hat.eval"]]
  pD0.hat[inds.eval.set[[i]]]<-pD.hat[["mu0.hat.eval"]]
}

f.hat = Z*ps.hat + (1-Z)*(1-ps.hat)

deltaD.hat = pD1.hat - pD0.hat
deltaY.hat = pY1.hat - pY0.hat

# estimator
IPW = (2*Z-1)/f.hat/deltaD.hat*Y
IPW = mean(IPW)

REG = deltaY.hat/deltaD.hat
REG = mean(REG)

MR = (2*Z-1)/f.hat/deltaD.hat*
  (Y- pY0.hat - (D-pD0.hat)*deltaY.hat/deltaD.hat ) +  deltaY.hat/deltaD.hat
MR = mean(MR)












