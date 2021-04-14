rm(list=ls())
library(parallel)

source("functionals.R")
source("TrueValue.R")
source("Data_gen.R")
source("plot.R")


## Monto Carlo times and Sample Size###
seed = 300
J <- 500
N <- 500
truevalue<- 0.087

## Monto Carlo times and Sample Size###
seed = 100
J <- 500
N <- 1000
truevalue<- 0.087


## Bandwidth
# h <- N^{-1/4}
# hopt <- N^{-1/5} #optimal bandwidth of univariate

# h1 <- N^{-1/4}
# h2 <- 0.85 * N^{-1/4}

# h3 <- 2.55 * N^{-1/4}
# ht <- 1.63 * N^{-1/5}


# parallel setting
cl <- makeCluster(50)
clusterExport(cl,ls())
# clusterEvalQ(cl,)

## Estimation function 
estimation <- function(count) {

  Data<-DataGen(N,seed+count)
  h <- N^{-2/7}
  hopt <- 1.06*sqrt(var(Data$x))* N^{-1/5}
  X<-Data$x
  Z<-Data$z
  D<-Data$d
  Y<-Data$y
  
  T1est <- T1(X,Y,D,Z,h)
  T2est <- T2(X,Y,D,Z,h)
  T3est <- T3(X,Y,D,Z,h)
  Test  <- Tt(X,Y,D,Z,hopt)
  veff  <- estVeff(X,Y,D,Z,hopt)
  
  est <- cbind(T1est,T2est,T3est,Test,veff)
  
  # est <- cbind(Test,veff)
  
  return(est)
}

est  <- parSapply(cl,1:J,estimation)
est  <- t(est)
veff <- est[,5]
est  <- est[,1:4]


result <- matrix(nrow = 4, ncol = 5)
colnames(result)<-c("bias","stdev","MSE","RMSE","CR")
rownames(result)<-c("T1","T2","T3","T")
for (i in 1:4) {
  Delta <- mean(est[,i])
  bias  <- Delta - truevalue
  mse   <- 1/J*(sum((est[,i]-truevalue)^2))
  stdev <- sqrt(1/J*(sum((est[,i]-Delta)^2)))
  rmse<- sqrt(mse)
  
  count<-0
  for(j in 1:J){
    if(est[j,i]> 0.087-1.96*stdev & est[j,i]< 0.087+1.96*stdev)
      count<- count+1
  }
  coverage_rate <<- count/J
  CR <- coverage_rate
  
  result[i,] <- cbind(bias,stdev,mse,rmse,CR)
}

result

# summary of veff
mean(veff)
sd(veff)
mean(veff/sqrt(N))

# plot
est.df <- data.frame(est)
plt(est.df)

stopCluster(cl)
