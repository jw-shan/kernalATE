source("3.1.1 MyFunc.R")
source("3.1.2 TrueValue.R")



DataGen = function(n,SEED){
  
  set.seed(SEED)
  
  # x: baseline covariates. 
  x0 = rep(1,n)  #intercept term
  x1 = rnorm(n,sd=0.1)
  x2 = rnorm(n,sd=0.1)
  x3 = abs(x1+x2) + abs(x1)*rnorm(n)
  x4 = abs(x1+x2)^2 + abs(x2)*rnorm(n)
  # x5 = rbinom(n,1,1/(1+exp(-x1)))
  # x6 = rbinom(n,1,dnorm(x2))
  x5 = rnorm(n,sd=0.1)
  x6 = rnorm(n,sd=0.1)

  # bind
  x            = cbind(x1,x2,x3,x4,x5,x6)
  x= scale(x)
  x = cbind(x0,x)
  u            = rbinom(n, 1, 0.5)
  
  ### True values depend on data
  pix.true     = expit(x %*% gamma.true)
  delta.true   = tanh(x %*% alpha.true)
  delta.d.true = tanh(x %*% beta.true)
  delta.y.true = delta.true * delta.d.true
  logOP.y.true = x %*% zeta.true 
  logOP.d.true = x %*% eta.true 
  
  p0p1.y.true = mapply(getProbScalarRDiff,atanh(delta.y.true),logOP.y.true)
  p0p1.d.true = mapply(getProbScalarRDiff,atanh(delta.d.true),logOP.d.true)
  
  # return(list(n=n, xs=xs, pix.true=pix.true, p0p1.d.true=p0p1.d.true,
  #             p0p1.y.true=p0p1.y.true, params.true=params.true,
  #             delta.d.true=delta.d.true))
  
  
  
  z = rbinom(n,1,pix.true)
  
  p.d.true = p0p1.d.true[1,]
  p.d.true[z==1] = p0p1.d.true[2,z==1]
  p.d.true = p.d.true + 0.1*(2*u-1)
  d = rbinom(n,1,p.d.true)
  
  p.y.true = p0p1.y.true[1,]
  p.y.true[z==1] = p0p1.y.true[2,z==1]
  p.y.true = p.y.true + 0.1*(2*u-1)
  y = rbinom(n,1,p.y.true)
  p.d.true = p0p1.d.true[1,]
  p.d.true[z==1] = p0p1.d.true[2,z==1]
  p.d.true = p.d.true + 0.1*(2*u-1)
  d = rbinom(n,1,p.d.true)
  
  p.y.true = p0p1.y.true[1,]
  p.y.true[z==1] = p0p1.y.true[2,z==1]
  p.y.true = p.y.true + 0.1*(2*u-1)
  y = rbinom(n,1,p.y.true)
  
  # Delta.oracle = numeric(length(method))
  # names(Delta.oracle) = method
  # delta.true = tanh(x %*% alpha.true)
  # Delta.oracle[c("reg","g")] = mean(delta.true)
  # f.z.x.true = pix.true 
  # f.z.x.true[z==0] = 1 - pix.true[z==0]
  # Delta.oracle[c("ipw","b-ipw")] = mean( y * (2*z-1) / 
  #                                           ( f.z.x.true * delta.d.true )  )
  # Delta.oracle[c("tr","b-tr")]  = mean(  ( y - d*delta.true - p0p1.y.true[1,] + 
  #                               delta.true * p0p1.d.true[1,]) *
  #                             (2 * z - 1)/ (f.z.x.true * delta.d.true) + 
  #                             delta.true  )
  
  return(list(x=x[,-1],z=z,d=d,y=y,delta.true=delta.true))
  
}



# # ## true ATE
# N=20000
# delta<- c(0)
# for(i in 1:500){
#  Data<-DataGen(N,100+i)
#  delta[i]<- mean(Data$delta.true)
# }
# mean(delta)

# # -0.047

