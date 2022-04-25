library(np)
npreg=npreg
npregbw = npregbw


KSE_CV <- function(X,Y,D,Z,h){
  n = length(Z)
  res = vector(length = n)
  
  X1 = X[Z==1]
  X0 = X[Z==0]
  Y1 = Y[Z==1]
  Y0 = Y[Z==0]
  D1 = D[Z==1]
  D0 = D[Z==0]
  
  # f(Z|X)
  fhat_bw = npregbw(Z~X)
  pi.hat = fitted(npreg(tydat=Z ,txdat=X, exdat=X, bws=h))
  f.hat = Z*pi.hat + (1-Z)*(1-pi.hat)
  
  # E(D|Z,X)
  pD1.hat_bw = npregbw(D1~X1)
  pD1.hat = fitted(npreg(tydat=D1 ,txdat=X1, exdat=X, bws=h))
  
  pD0.hat_bw = npregbw(D0~X0)
  pD0.hat = fitted(npreg(tydat=D0 ,txdat=X0, exdat=X, bws=h))
  
  # E(Y|Z,X)
  pY1.hat_bw = npregbw(Y1~X1)
  pY1.hat = fitted(npreg(tydat=Y1 ,txdat=X1, exdat=X, bws=h))
  
  pY0.hat_bw = npregbw(Y0~X0)
  pY0.hat = fitted(npreg(tydat=Y0 ,txdat=X0, exdat=X, bws=h))
  
  
  # estimator
  deltaD.hat = pD1.hat - pD0.hat
  deltaY.hat = pY1.hat - pY0.hat
  
  res = (2*Z-1)/f.hat/deltaD.hat*
    (Y- pY0.hat - (D-pD0.hat)*deltaY.hat/deltaD.hat ) +  deltaY.hat/deltaD.hat
  res = mean(res)
  
  return(res)
  
  
  
  
  
  
}