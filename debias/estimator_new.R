

estimate_reg<-function(Y,Z,X,inds.train=NULL,inds.eval=NULL,...) {
  # This function estimates the conditional mean of Y given Z and X
  
  N<-length(Z)
  p<-dim(X)[2]
  if (is.null(inds.train)|is.null(inds.eval)) {inds.train = inds.eval = 1:N}
  N.eval = length(inds.eval)
  
  # Estimated regression function
  mu1.hat <- rep(0,N.eval)
  mu0.hat <- rep(0,N.eval)
  
  rf.data <- data.frame(Y=Y,Z=Z,X=X)[inds.train,]
  rf.reg <- randomForest(Y~.,rf.data)
  
  mu1.hat <- predict(rf.reg, newdata =data.frame(Z=rep(1,N.eval),X=X[inds.eval,]))
  mu0.hat <- predict(rf.reg, newdata =data.frame(Z=rep(0,N.eval),X=X[inds.eval,]))
  
  
  return(list(mu1.hat.eval=mu1.hat,mu0.hat.eval=mu0.hat))
}




estimate_ps<-function(Z,X,inds.train=NULL,inds.eval=NULL,...) {
  # This function estimates the conditional mean of Y given Z and X
  
  N<-length(Z)
  p<-dim(X)[2]
  if (is.null(inds.train)|is.null(inds.eval)) {inds.train = inds.eval = 1:N}
  N.eval = length(inds.eval)
  
  # Estimated regression function
  ps.hat <- rep(0,N.eval)
  
  rf.data <- data.frame(Z=Z,X=X)[inds.train,]
  rf.reg <- randomForest(Z~.,rf.data)
  ps.hat <- predict(rf.reg, newdata =data.frame(X=X[inds.eval,]))

  
  return(ps.hat)
}



estimate_ATE <-function(count,n,p,p1=4,K=2,J,SEED){
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
  
  
  # data splitting
  # K=2
  inds.train.set = list()
  inds.eval.set = list()
  inds = 1:n
  for (i in 1:K) {
    inds.eval.set[[i]] <- sample(inds,floor(length(inds)/(K-i+1)))
    inds.train.set[[i]] <- setdiff(1:n,inds.eval.set[[i]])
    inds = setdiff(inds,inds.eval.set[[i]])
  }
  
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
  
  est <- cbind(IPW,REG,MR)
  
  return(est)
}


