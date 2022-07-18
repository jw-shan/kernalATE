# random forest estimation
library(randomForest)

fhat <- function(X,Z,h){
  n = length(Z)
  res = vector(length = n)
  
  data.rf <- data.frame(X=X)
  fitted.rf<-randomForest(data.rf,Z, maxnodes = 10)
  pi.hat<-predict(fitted.rf,data.rf)
  # plot(X,pi.hat)
  # plot(pi.hat,Data$pix.true)
  res = Z*pi.hat + (1-Z)*(1-pi.hat)
  return(res)
}

pDhat <- function(ind,X,Z,D,h){
  n = length(Z)
  res = vector(length = n)
  
  
  data.rf <- data.frame(X=X,Z=Z)
  data.rf.ind <- data.frame(X=X,Z=ind)
  fitted.rf<-randomForest(data.rf,D, maxnodes = 10)
  res <- predict(fitted.rf,data.rf.ind)
  res <- as.vector(res)
  
  plot(X,res)
  
  return(res)
}

pYhat <- function(ind,X,Z,Y,h){
  n = length(Z)
  res = vector(length = n)
  
  data.rf <- data.frame(X=X,Z=Z)
  data.rf.ind <- data.frame(X=X,Z=ind)
  fitted.rf<-randomForest(data.rf,Y, maxnodes = 10)
  res <- predict(fitted.rf,data.rf.ind)
  res <- as.vector(res)
  
  return(res)
}

deltaDhat <- function(X,Z,D,h){
  return(pDhat(1,X,Z,D,h)-pDhat(0,X,Z,D,h))
}

deltaYhat <- function(X,Z,Y,h){
  return(pYhat(1,X,Z,Y,h)-pYhat(0,X,Z,Y,h))
}

# Estimators
KSE_0 <- function(X,Y,D,Z,h){
  n = length(Z)
  sum( (2*Z-1)/fhat(X,Z,h)*
             (pDhat(0,X,Z,D,h)* deltaYhat(X,Z,Y,h) / (deltaDhat(X,Z,D,h))^2 -
                pYhat(0,X,Z,Y,h) / deltaDhat(X,Z,D,h)) )/n
  }

KSE_1 <- function(X,Y,D,Z,h){
  n = length(Z)
  sum( (2*Z-1)/fhat(X,Z,h)*
             Y/deltaDhat(X,Z,D,h) ) /n
  }


KSE_2 <- function(X,Y,D,Z,h){
  n = length(Z)
  sum( (2*Z-1)/fhat(X,Z,h)*
             D*deltaYhat(X,Z,Y,h) / (deltaDhat(X,Z,D,h))^2 ) /n
  }

KSE_3 <- function(X,Y,D,Z,h){
  n = length(Z)
  sum( deltaYhat(X,Z,Y,h)/deltaDhat(X,Z,D,h) ) /n}

KSE_t <- function(X,Y,D,Z,h){KSE_0(X,Y,D,Z,h) + KSE_1(X,Y,D,Z,h) - KSE_2(X,Y,D,Z,h) + KSE_3(X,Y,D,Z,h)}

estVeff <- function(X,Y,D,Z,h){
  n = length(Z)
  phi_i =  (2*Z-1)/fhat(X,Z,h)*
         (Y/deltaDhat(X,Z,D,h) - D*deltaYhat(X,Z,Y,h) / (deltaDhat(X,Z,D,h))^2 +
            pDhat(0,X,Z,D,h)* deltaYhat(X,Z,Y,h) / (deltaDhat(X,Z,D,h))^2 -
            pYhat(0,X,Z,Y,h) / deltaDhat(X,Z,D,h)) +
         deltaYhat(X,Z,Y,h)/deltaDhat(X,Z,D,h) 
  sqrt(sum( (phi_i -  sum(phi_i)/n)^2  )/n )
}
