
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
  fhat_bw = npregbw(Z~X,regtype="ll")
  pi.hat = fitted(npreg(tydat=Z ,txdat=X, exdat=X, bws=fhat_bw[[1]]),regtype="ll")
  f.hat = Z*pi.hat + (1-Z)*(1-pi.hat)
  # plot(Data$pix.true,pi.hat)
  
  # E(D|Z,X)
  pD.hat_bw = npregbw(D~X+Z,regtype="ll")[[1]][1]
  pD0.hat = fitted(npreg(tydat=D0 ,txdat=X0, exdat=X, bws=pD.hat_bw,regtype="ll"))
  pD1.hat = fitted(npreg(tydat=D1 ,txdat=X1, exdat=X, bws=pD.hat_bw,regtype="ll"))
  # plot(Data$p0p1.d.true[1,],pD0.hat)
  # plot(Data$p0p1.d.true[2,],pD1.hat)
  
  
  
  # E(Y|Z,X)
  pY.hat_bw = npregbw(Y~X+Z,regtype="ll")[[1]][1]
  pY0.hat = fitted(npreg(tydat=Y0 ,txdat=X0, exdat=X, bws=pY.hat_bw,regtype="ll"))
  pY1.hat = fitted(npreg(tydat=Y1 ,txdat=X1, exdat=X, bws=pY.hat_bw,regtype="ll"))
  # plot(Data$p0p1.y.true[1,],pY0.hat)
  # plot(Data$p0p1.y.true[2,],pY1.hat)
  
  # estimator
  deltaD.hat = pD1.hat - pD0.hat
  deltaY.hat = pY1.hat - pY0.hat
  
  res = (2*Z-1)/f.hat/deltaD.hat*
    (Y- pY0.hat - (D-pD0.hat)*deltaY.hat/deltaD.hat ) +  deltaY.hat/deltaD.hat
  res = mean(res)
  
  return(res)
  
  
  
  
  
  
}