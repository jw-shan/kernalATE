# This function implements cross-validation with the help of "lokern" packages.

 KSE_CV <- function(X,Y,D,Z){
  n = length(Z)
  res = vector(length = n)
  
  X1 = X[Z==1]
  X0 = X[Z==0]
  
  Y1 = Y[Z==1]
  Y0 = Y[Z==0]
  D1 = D[Z==1]
  D0 = D[Z==0]
  
  # f(Z|X)
  pi.hat = glkerns(Z~X,x.out = X)
  # plot(pi.hat$x.out, pi.hat$est)
  pi.hat = pi.hat$est[match(X,pi.hat$x.out)]
  # plot(Data$pix.true,pi.hat)
  f.hat = Z*pi.hat + (1-Z)*(1-pi.hat)
  
  # E(D|Z,X)
  pD1.hat = glkerns(D1~X1,x.out = X)
  pD1.hat = pD1.hat$est[match(X,pD1.hat$x.out)]
  # plot(Data$p0p1.d.true[2,],pD1.hat)
  
  pD0.hat = glkerns(D0~X0,x.out = X)
  pD0.hat = pD0.hat$est[match(X,pD0.hat$x.out)]
  # plot(Data$p0p1.d.true[1,],pD0.hat)
  
  # E(Y|Z,X)
  pY1.hat = glkerns(Y1~X1,x.out = X)
  pY1.hat = pY1.hat$est[match(X,pY1.hat$x.out)]
  
  pY0.hat = glkerns(Y0~X0,x.out = X)
  pY0.hat = pY0.hat$est[match(X,pY0.hat$x.out)]
  
  
  # estimator
  deltaD.hat = pD1.hat - pD0.hat
  deltaY.hat = pY1.hat - pY0.hat
  
  res = (2*Z-1)/f.hat/deltaD.hat*
    (Y- pY0.hat - (D-pD0.hat)*deltaY.hat/deltaD.hat ) +  deltaY.hat/deltaD.hat
  res = mean(res)
  
  return(res)
  
  
  
  
  
  
}