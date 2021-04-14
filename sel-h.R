rm(list=ls())
library(parallel)

source("functionals.R")
source("TrueValue.R")
source("Data_gen.R")


## Monto Carlo times and Sample Size###
seed = 100
J <- 500
N <- 500
truevalue<- 0.087

cl <- makeCluster(50)
clusterExport(cl,ls())

## Bandwidth
h1 <- seq(0.5 *N^{-1/4}, 2.5 *N^{-1/4}, length.out = 75)
h2 <- seq(1 * N^{-1/5}, 3 * N^{-1/5}, length.out = 75)

result500 <- data.frame(num = 1:75, 
                         t1_bias = rep(NA,75), t2_bias = rep(NA,75),t3_bias = rep(NA,75),t_bias = rep(NA,75),
                         t1_std = rep(NA,75), t2_std = rep(NA,75),t3_std = rep(NA,75),t_std = rep(NA,75),
                         t1_mse = rep(NA,75), t2_mse = rep(NA,75),t3_mse = rep(NA,75),t_mse = rep(NA,75))

for (i in 1:75) {
  print(i)
  hh1 <- h1[i]
  hh2 <- h2[i]
  clusterExport(cl,c("hh1","hh2"))
  
  estimation <- function(count) {
    
    Data<-DataGen(N,seed+count)
    X<-Data$x
    Z<-Data$z
    D<-Data$d
    Y<-Data$y
    
    T1est <- T1(X,Y,D,Z,hh1)
    T2est <- T2(X,Y,D,Z,hh1)
    T3est <- T3(X,Y,D,Z,hh1)
    Test  <- Tt(X,Y,D,Z,hh2)
    
    est <- cbind(T1est,T2est,T3est,Test)
    
    return(est)
  }
  
  est <- parSapply(cl,1:J,estimation)
  est <- t(est)
  
  for (j in 1:4) {
    Delta <- mean(est[,j])
    bias  <- Delta - truevalue
    stdev <- sqrt(1/J*(sum((est[,j]-Delta)^2)))
    mse   <- 1/J*(sum((est[,j]-truevalue)^2))
    result500[i,j+1] <- bias
    result500[i,j+5] <- stdev
    result500[i,j+9] <- mse
    }
}

stopCluster(cl)

save.image("~/Research/kernalATE/result500.RData")