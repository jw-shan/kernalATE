rm(list=ls())
load("3.1 NLS.RData")
source("3.2 KernelEstimators.R")
library(np)


### Data cleaning
X = X.mat[,c(2:4, 8,9,12, 14)]


band = npregbw(Z~X)






















X$age = scale(X$age)
X$iq  = scale(X$iq)
X$parentseduc = scale(X$motheduc+X$fatheduc)



### romove f(z|x)=0 
K_x0 = K(X,h)
ind0 <- vector()
for (i in 1:N) {
  f_1 <- K_x0[,i] * (Z==1) 
  f_0 <- K_x0[,i] * (Z==0)
  if (sum(f_1)==0 | sum(f_0)==0) {
    ind0 = c(ind0,i)
  }
}
X <- X[-ind0,]
Y <- Y[-ind0]
Z <- Z[-ind0]
D <- D[-ind0]
N <- length(D)

### remove delta^D=0
h <- 1.06* N^{-1/7}
K_x0 = K(X,h)
ind0 = which(abs(deltaDhat(X,Z,D,h,K_x0))<1e-6)
X <- X[-ind0,]
Y <- Y[-ind0]
Z <- Z[-ind0]
D <- D[-ind0]
N <- length(D)


K_x = K(X,h)
sum( (2*Z-1)/fhat(X,Z,h,K_x)*
       Y/deltaDhat(X,Z,D,h,K_x) ) /N



## Estimation function 



h <- N^{-1/7}
KSET_est <- KSE_t(X,Y,D,Z,h)


KSE1_est <- KSE_1(X,Y,D,Z,h)
KSE2_est <- KSE_2(X,Y,D,Z,h)
KSE3_est <- KSE_3(X,Y,D,Z,h)
KSET_est <- KSE_t(X,Y,D,Z,h)
# veff  <- estVeff(X,Y,D,Z,hopt)

# est <- cbind(T1est,T2est,T3est,Test,veff)
est <- cbind(KSE1_est,KSE2_est,KSE3_est,KSET_est)



est  <- parSapply(cl,1:J,estimation)
est  <- t(est)

# veff <- est[,5]
# est  <- est[,1:4]


result <- matrix(nrow = 4, ncol = 4)
colnames(result)<-c("bias","stdev","RMSE","CR")
rownames(result)<-c("KSE-1","KSE-2","KSE-3","KSE-T")
for (i in 1:4) {
  Delta <- mean(est[,i])
  bias  <- Delta - truevalue
  mse   <- 1/J*(sum((est[,i]-truevalue)^2))
  stdev <- sqrt(1/J*(sum((est[,i]-Delta)^2)))
  rmse<- sqrt(mse)
  
  count<-0
  for(j in 1:J){
    if(est[j,i]> truevalue-1.96*stdev & est[j,i]< truevalue+1.96*stdev)
      count<- count+1
  }
  coverage_rate <<- count/J
  CR <- coverage_rate
  
  result[i,] <- cbind(bias,stdev,rmse,CR)
}

result

save(est,result,N,seed,file = paste0("2.4.1 Result_N=",as.character(N),"_seed=",as.character(seed),".RData"))

# # summary of veff
# mean(veff)
# sd(veff)
# mean(veff/sqrt(N))


# plot
est.df <- data.frame(est)
plt_ATE(est.df)

stopCluster(cl)





# ======naive=====================================================================
estimation <- function(count) {
  
  Data<-DataGen(N,seed+count)
  X<-Data$x
  Z<-Data$z
  D<-Data$d
  Y<-Data$y
  
  est <- mean(Y[D==1])-mean(Y[D==0])
  
  # est <- cbind(Test,veff)
  
  return(est)
}

est  <- parSapply(cl,1:J,estimation)
est  <- t(est)

result <- matrix(nrow = 1, ncol = 5)
colnames(result)<-c("bias","stdev","MSE","RMSE","CR")
rownames(result)<-"naive"

Delta <- mean(est)
bias  <- Delta - truevalue
mse   <- 1/J*(sum((est-truevalue)^2))
stdev <- sqrt(1/J*(sum((est-Delta)^2)))
rmse<- sqrt(mse)

count<-0
for(j in 1:J){
  if(est[j]> truevalue-1.96*stdev & est[j]< truevalue+1.96*stdev)
    count<- count+1
}
coverage_rate <<- count/J
CR <- coverage_rate

result <- cbind(bias,stdev,mse,rmse,CR)


result



