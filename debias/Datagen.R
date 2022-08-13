# source("1.1.1 MyFunc.R")
# source("1.1.2 TrueValue.R")

library(MASS)

expit <- function(logodds){ 1/(1+exp(-logodds))}


Datagen = function(SEED,n,p=500,p1=4){
  
  set.seed(SEED)
  
  
  beta.true <- c(rep(1,p1),rep(0,p-p1))   # Y
  gamma.true <- c(rep(-0.3,p1),rep(0,p-p1))  # Z
  eta.true <- c(rep(0.5,p1),rep(0,p-p1))  # D1
  
  kappa = 0.1
  u  = rbinom(n, 1, 0.5)
  
  X <- rnorm(n*p)
  X <- matrix(X,n,p)  #X~N(0,I_p)
  
  pix.true <- expit(0.1 + X %*% gamma.true)
  Z <- rbinom(n,1,pix.true)
  # Z.true <- Z*pix.true + (1-Z)*(1-pix.true)
  
  Y.0.true <- rep(0,n)
  Y.1.true <- 10 + X %*% beta.true 
  Y.0 <- Y.0.true + kappa*(2*u-1)
  Y.1 <- Y.1.true + kappa*(2*u-1)
  Y.1 <- as.numeric(Y.1)
  Y <- Z*Y.1+(1-Z)*Y.0
  
  D.0.true <- 0.2 
  D.1.true <- 0.7 + 0.1 * (2*expit(X%*%eta.true)-1) 
  D.0 <- rbinom(n, 1, D.0.true + kappa*(2*u-1))
  D.1 <- rbinom(n, 1, D.1.true + kappa*(2*u-1))
  D <- Z*D.1+(1-Z)*D.0
  
  delta.true = (Y.1.true - Y.0.true)/(D.1.true - D.0.true)
  

  
  
  return(list(X=X,Y=Y,Z=Z,D=D,Y.1.true=Y.1.true,Y.0.true=Y.0.true,D.1.true=D.1.true,D.0.true=D.0.true,pix.true=pix.true,delta.true=delta.true))
  
  
  # 
  # 
  # 
  # 
  # p = 10
  # # x: baseline covariates. 
  # x1           = rep(1,n)  #intercept term
  # x2.ind       = rbinom(n,1,0.5)
  # x2           = ((-1)^x2.ind) * runif(n,0.5,1)
  # 
  # #x2            =  runif(n,0.5,1)
  # x            = cbind(x1,x2)
  # u            = rbinom(n, 1, 0.5)
  # 
  # # auxillary covariate
  # sig = 0.5^abs(outer(1:(p-1),1:(p-1),"-"))
  # xa = mvrnorm(n,mu=rep(0,p-1),Sigma = sig)
  # x.ob = cbind(x2,xa)
  # 
  # 
  # ### True values depend on data
  # pix.true     = expit(x %*% gamma.true)
  # delta.true   = tanh(x %*% alpha.true)
  # delta.d.true = tanh(x %*% beta.true)
  # delta.y.true = delta.true * delta.d.true
  # logOP.y.true = x %*% zeta.true 
  # logOP.d.true = x %*% eta.true 
  # 
  # p0p1.y.true = mapply(getProbScalarRDiff,atanh(delta.y.true),logOP.y.true)
  # p0p1.d.true = mapply(getProbScalarRDiff,atanh(delta.d.true),logOP.d.true)
  # 
  # # return(list(n=n, xs=xs, pix.true=pix.true, p0p1.d.true=p0p1.d.true,
  # #             p0p1.y.true=p0p1.y.true, params.true=params.true,
  # #             delta.d.true=delta.d.true))
  # 
  # 
  # 
  # z = rbinom(n,1,pix.true)
  # 
  # p.d.true = p0p1.d.true[1,]
  # p.d.true[z==1] = p0p1.d.true[2,z==1]
  # p.d.true = p.d.true + 0.1*(2*u-1)
  # d = rbinom(n,1,p.d.true)
  # 
  # p.y.true = p0p1.y.true[1,]
  # p.y.true[z==1] = p0p1.y.true[2,z==1]
  # p.y.true = p.y.true + 0.1*(2*u-1)
  # y = rbinom(n,1,p.y.true)
  # p.d.true = p0p1.d.true[1,]
  # p.d.true[z==1] = p0p1.d.true[2,z==1]
  # p.d.true = p.d.true + 0.1*(2*u-1)
  # d = rbinom(n,1,p.d.true)
  # 
  # p.y.true = p0p1.y.true[1,]
  # p.y.true[z==1] = p0p1.y.true[2,z==1]
  # p.y.true = p.y.true + 0.1*(2*u-1)
  # y = rbinom(n,1,p.y.true)
  # 
  # # Delta.oracle = numeric(length(method))
  # # names(Delta.oracle) = method
  # # delta.true = tanh(x %*% alpha.true)
  # # Delta.oracle[c("reg","g")] = mean(delta.true)
  # # f.z.x.true = pix.true 
  # # f.z.x.true[z==0] = 1 - pix.true[z==0]
  # # Delta.oracle[c("ipw","b-ipw")] = mean( y * (2*z-1) / 
  # #                                           ( f.z.x.true * delta.d.true )  )
  # # Delta.oracle[c("tr","b-tr")]  = mean(  ( y - d*delta.true - p0p1.y.true[1,] + 
  # #                               delta.true * p0p1.d.true[1,]) *
  # #                             (2 * z - 1)/ (f.z.x.true * delta.d.true) + 
  # #                             delta.true  )
  # 
  # return(list(x=x.ob,z=z,d=d,y=y,pix.true=pix.true, p0p1.d.true=p0p1.d.true,
  #             p0p1.y.true=p0p1.y.true,delta.true=delta.true))
  # 
}



# # # ## true ATE
# library(parallel)
# ncores = detectCores()
# if (ncores<40) {
#   cl = makeCluster(ncores)
# }else{
#   cl = makeCluster(40)
# }
# 
# N=30000
# J=1000
# delta<- rep(0,J)
# clusterExport(cl,ls())
# 
# comp <- function(count) {
#  Data<-DataGen(N,1000+count)
#  mean(Data$delta.true)
# }
# delta  <- parSapply(cl,1:J,comp)
# mean(delta)

