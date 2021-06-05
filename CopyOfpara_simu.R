# setwd("~/Research/DML/")
rm(list=ls())
library(parallel)
library(dplyr)
library(randomForest)

options (warn = -1)

# source("functionals.R")
source("TrueValue.R")
source("Data_Gen_10D.R")
# source("plot.R")


## Monto Carlo times and Sample Size###
seed = 100
K <- 2
J <- 500
N <- 10000
truevalue<- 0.658


# parallel setting
cl <- makeCluster(50)
randomForest = randomForest
select = select 

clusterExport(cl,ls())
# clusterEvalQ(cl, "randomForest")

## Estimation function 
estimation <- function(count) {

  Data<-DataGen(N,seed+count)
  Data.sp <- DataSplit(Data,K,seed+count)
  
  theta.hat <- vector(length = K)
  for (k in 1:K) {
    # Estimate nuisance
    Data.nui.list <- Data.sp
    Data.nui.list[[k]] <- NULL
    Data.nui <- data.frame()
    for (j in 1:(K-1)) {
      Data.nui <- rbind(Data.nui,Data.nui.list[[j]])
    }
    
    Data.nui.z1 <- Data.nui[Data.nui$z==1,]
    Data.nui.z0 <- Data.nui[Data.nui$z==0,]
    
    
    zofx  <- randomForest(z~., data = select(Data.nui, c(x.1:x.11,z)), mtry = 3)
    yofx1 <- randomForest(y~., data = select(Data.nui.z1, c(x.1:x.11,y)), mtry = 3)
    yofx0 <- randomForest(y~., data = select(Data.nui.z0, c(x.1:x.11,y)), mtry = 3)
    dofx1 <- randomForest(d~., data = select(Data.nui.z1, c(x.1:x.11,d)), mtry = 3)
    dofx0 <- randomForest(d~., data = select(Data.nui.z0, c(x.1:x.11,d)), mtry = 3)
    
    # Estimate interest
    Data.int <- Data.sp[[k]]
    Data.int.zofx  <- predict(zofx, select(Data.int, x.1:x.11))
    Data.int.yofx1 <- predict(yofx1, select(Data.int, x.1:x.11))
    Data.int.yofx0 <- predict(yofx0, select(Data.int, x.1:x.11))
    Data.int.dofx1 <- predict(dofx1, select(Data.int, x.1:x.11))
    Data.int.dofx0 <- predict(dofx0, select(Data.int, x.1:x.11))
    y <- Data.int$y 
    z <- Data.int$z
    d <- Data.int$d

    n = length(z)
    theta.hat[k] <- sum(
     ( z*(y-Data.int.yofx1)/Data.int.zofx - (1-z)*(y-Data.int.yofx0)/(1 - Data.int.zofx) + Data.int.yofx1 - Data.int.yofx0 ) / ( Data.int.dofx1 - Data.int.dofx0 ) +
     ( z*(d-Data.int.dofx1)/Data.int.zofx - (1-z)*(d-Data.int.dofx0)/(1 - Data.int.zofx) ) * ( Data.int.yofx1 - Data.int.yofx0 ) / ( Data.int.dofx1 - Data.int.dofx0 )
    )/n
  }
  return(theta.hat)
}

est  <- parSapply(cl,1:J,estimation)
save.image("~/Research/DML/N=10000K=2.RData")


# est  <- t(est)
# veff <- est[,5]
# est  <- est[,1:4]
# 
# 
# result <- matrix(nrow = 4, ncol = 5)
# colnames(result)<-c("bias","stdev","MSE","RMSE","CR")
# rownames(result)<-c("T1","T2","T3","T")
# for (i in 1:4) {
#   Delta <- mean(est[,i])
#   bias  <- Delta - truevalue
#   mse   <- 1/J*(sum((est[,i]-truevalue)^2))
#   stdev <- sqrt(1/J*(sum((est[,i]-Delta)^2)))
#   rmse<- sqrt(mse)
#   
#   count<-0
#   for(j in 1:J){
#     if(est[j,i]> 0.087-1.96*stdev & est[j,i]< 0.087+1.96*stdev)
#       count<- count+1
#   }
#   coverage.rate <<- count/J
#   CR <- coverage.rate
#   
#   result[i,] <- cbind(bias,stdev,mse,rmse,CR)
# }
# 
# result
# 
# # summary of veff
# mean(veff)
# sd(veff)
# mean(veff/sqrt(N))
# 
# # plot
# est.df <- data.frame(est)
# plt(est.df)

stopCluster(cl)
