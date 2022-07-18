

estimate_mu<-function(Y,Z,X,inds.train,inds.eval,method="lasso",...) {
  # This function estimates the conditional mean of Y/Z given Z and X
  
  N<-length(Z)
  p<-dim(X)[2]
  N.eval = length(inds.eval)
  
  # Estimated regression function
  mu1.hat <- rep(0,N.eval)
  mu0.hat <- rep(0,N.eval)
  
  
  
  if (method=="lasso") {
    
    X1 <- data.frame(X=X,Z=Z)
    colnames(X1)[1:p] <- paste0("X",1:p)
    
    fo <- as.formula(paste(c("~0+Z+Z:(+",paste0("X",1:p,collapse = "+"),")+I(1-Z)+I(1-Z):(",paste0("X",1:p,collapse = "+"),")"),collapse = ""))
    regressor = model.matrix(fo,X1)
    
    
    ###train
    lasso.reg <- rlassologit(Y~regressor,subset=inds.train,intercept = FALSE)
    
    ### estimate mu1 on ind.eval
    X1.1 <- X1[inds.eval,]
    X1.1$Z = rep(1,N.eval)
    regressor.1 = model.matrix(fo,X1.1)
    
    mu1.hat <- predict(lasso.reg, newdata = regressor.1)
    
    ### estimate mu0 on ind.eval
    X1.0 <- X1[inds.eval,]
    X1.0$Z = rep(0,N.eval)
    regressor.0 = model.matrix(fo,X1.0)
    
    mu0.hat <- predict(lasso.reg, newdata = regressor.0)
    
  } else if (method == "rf") {
    rf.data <- data.frame(Y=Y,Z=Z,X=X)[inds.train,]
    rf.reg <- randomForest(Y~.,rf.data)
    
    mu1.hat <- predict(rf.reg, newdata =data.frame(Z=rep(1,N.eval),X=X[inds.eval,]))
    mu0.hat <- predict(rf.reg, newdata =data.frame(Z=rep(0,N.eval),X=X[inds.eval,]))
  }
  
  
  return(list(mu1.hat.eval=mu1.hat,mu0.hat.eval=mu0.hat))
}



estimate_ps<-function(Z,X,inds.train,inds.eval,method="lasso",...){
  ## This function estimates the propensty score--the conditional mean of Z given X
  ## output: the estimated valued in inds.eval set              
  
  
  N<-length(Z)
  p<-dim(X)[2]
  N.eval = length(inds.eval)
  
  # Estimated regression function
  ps.hat<-rep(0,N.eval)
  
  
  if (method == "lasso") {
    
    ## rlassologit
    lasso.reg <- rlassologit(X,Z,subset=inds.train)
    ps.hat <- predict(lasso.reg, newdata = X[inds.eval,])
    
  } else if (method == "rf") {
    
    rf.data <- data.frame(Z=factor(Z),X=X)[inds.train,]
    rf.reg <- randomForest(Z~.,rf.data,mtry=p/3,ntree=1000,nodesize=10)
    ps.hat <- predict(rf.reg, newdata =data.frame(X=X[inds.eval,]),type = "prob")[,2]
  }
  
  ps.hat[which(ps.hat<0.01)]=0.01
  ps.hat[which(ps.hat>0.99)]=0.99
  
  
  # !!!!note that the order of ps.hat is corressponding to the order of inds.eval.
  
  return(ps.hat)
  
}
