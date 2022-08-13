# This function implements cross-validation with the help of "np" packages.

 KSE_CV <- function(X,Y,D,Z){
  n = length(Z)
  
  X1 = X[Z==1]
  X0 = X[Z==0]
  Y1 = Y[Z==1]
  Y0 = Y[Z==0]
  D1 = D[Z==1]
  D0 = D[Z==0]
  
  # f(Z|X)
  fhat_bw = npregbw(Z~X)
  pi.hat = fitted(npreg(tydat=Z ,txdat=X, exdat=X, bws=fhat_bw))
  f.hat = Z*pi.hat + (1-Z)*(1-pi.hat)
  
  # E(D|Z,X)
  pD1.hat_bw = npregbw(D1~X1)
  pD1.hat = fitted(npreg(tydat=D1 ,txdat=X1, exdat=X, bws=pD1.hat_bw))
  
  pD0.hat_bw = npregbw(D0~X0)
  pD0.hat = fitted(npreg(tydat=D0 ,txdat=X0, exdat=X, bws=pD0.hat_bw))
  
  # E(Y|Z,X)
  pY1.hat_bw = npregbw(Y1~X1)
  pY1.hat = fitted(npreg(tydat=Y1 ,txdat=X1, exdat=X, bws=pY1.hat_bw))
  
  pY0.hat_bw = npregbw(Y0~X0)
  pY0.hat = fitted(npreg(tydat=Y0 ,txdat=X0, exdat=X, bws=pY0.hat_bw))
  
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
  
  res = list(IPW=IPW,REG=REG,MR=MR)
  
  return(res)
  
  
 }
 
 
 
 RF <- function(X,Y,D,Z){
   n = length(Z)
   
   
   # f(Z|X)
   data.x.rf <- data.frame(X=X)
   fitted.rf<-randomForest(data.x.rf,Z, maxnode=10)
   # fitted.rf<-randomForest(data.x.rf,Z)
   pi.hat<-predict(fitted.rf,data.x.rf)
   f.hat = Z*pi.hat + (1-Z)*(1-pi.hat)
   plot(Data$pix.true,pi.hat)
   
   
   
   # E(D|Z,X) and E(Y|Z,X)
   data.xz.rf <- data.frame(X=X,Z=Z)
   data.xz.rf.1 <- data.frame(X=X,Z=1)
   data.xz.rf.0 <- data.frame(X=X,Z=0)
   
   pD.hat_fit = randomForest(data.xz.rf,D,maxnode=10)
   pD.hat_fit = randomForest(data.xz.rf,D)
   pD1.hat = predict(pD.hat_fit,data.xz.rf.1)
   pD0.hat = predict(pD.hat_fit,data.xz.rf.0)
   plot(Data$p0p1.d.true[1,],pD1.hat)
   
   
   
   pY.hat_fit = randomForest(data.xz.rf,Y,maxnode=10)
   # pY.hat_fit = randomForest(data.xz.rf,Y)
   pY1.hat = predict(pY.hat_fit,data.xz.rf.1)
   pY0.hat = predict(pY.hat_fit,data.xz.rf.0)
   
   
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
   
   res = list(IPW=IPW,REG=REG,MR=MR)
   
   return(res)
   
   
 }
 
 # plot(Data$pix.true,pi.hat)
 # abline(a=0,b=1,col="red")