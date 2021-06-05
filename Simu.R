source("functionals.R")
source("TrueValue.R")
source("Data_gen.R")


truevalue<- 0.087
## Monto Carlo times and Sample Size###
J <- 500
N <- 1000

## Bandwidth
h <- N^{-5/16}
hopt <- N^{-1/5} #optimal bandwidth of univariate

## Initialization
count <- 0
# T0est <- vector()
T1est <- vector()
T2est <- vector()
T3est <- vector()
Test  <- vector()



est <- cbind(T1est,T2est,T3est,Test)
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
  return(est)
}

# 
# ## Estimation
# repeat {
#   if (count == J) break
#   count <- count + 1
#   print(count)
#   
#   Data<-DataGen(N,2021+count)
#   X<-Data$x
#   Z<-Data$z
#   D<-Data$d
#   Y<-Data$y
# 
#   
#   T1est[count] <- T1(X,Y,D,Z,h)
#   T2est[count] <- T2(X,Y,D,Z,h)
#   T3est[count] <- T3(X,Y,D,Z,h)
#   Test[count]  <- Tt(X,Y,D,Z,hopt)
# }
# 
# est <- cbind(T1est,T2est,T3est,Test)
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
#   coverage_rate <<- count/J
#   CR <- coverage_rate
#   result[i,] <- cbind(bias,stdev,mse,rmse,CR)
# }
# result
