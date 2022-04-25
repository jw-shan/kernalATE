library(np)
npreg=npreg
npregbw = npregbw


KSE_CV <- function(X,Y,D,Z){
  n = length(Z)
  res = vector(length = n)
  
  # f(Z|X)
  fhat_bw = npregbw(Z~X)
  pi.hat = fitted(npreg(fhat_bw,exdat=X))
  f.hat = Z*pi.hat + (1-Z)*(1-pi.hat)
  
  # E(D|Z,X)
  pD1.hat_bw = npregbw(D~X,subset = (Z==1))
  pD1.hat = fitted(npreg(pD1.hat_bw,exdat=X))
  
  pD0.hat_bw = npregbw(D~X,subset = (Z==0))
  pD0.hat = fitted(npreg(pD0.hat_bw,exdat=X))
  
  # E(Y|Z,X)
  pY1.hat_bw = npregbw(Y~X,subset = (Z==1))
  pY1.hat = fitted(npreg(pD1.hat_bw,exdat=X))
  
  pY0.hat_bw = npregbw(Y~X,subset = (Z==0))
  pY0.hat = fitted(npreg(pY0.hat_bw,exdat=X))
  
  
  # estimator
  deltaD.hat = pD1.hat - pD0.hat
  deltaY.hat = pY1.hat - pY0.hat
  
  res = (2*Z-1)/f.hat/deltaD.hat*
    (Y- pY0.hat - (D-pD0.hat)*deltaY.hat/deltaD.hat ) +  deltaY.hat/deltaD.hat
  res = mean(res)
  
  return(res)
  
  
  
  
  
  
}