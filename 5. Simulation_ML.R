rm(list=ls())
library(parallel)
library(reshape2)

source("1.1 Datagen_1D.R")
source("5.2 RFEstimators.R")
source("5.3 plot.R")


## Monto Carlo times and Sample Size###
seed = 11
J <- 500
N <- 500
truevalue<- 0.087

# ## Monto Carlo times and Sample Size###
# seed = 11
# J <- 500
# N <- 1000
# truevalue<- 0.087



# parallel setting
# configure parallel environment
ncores = detectCores()
if (ncores<40) {
  cl = makeCluster(ncores)
}else{
  cl = makeCluster(40)
}
clusterExport(cl,ls())
clusterEvalQ(cl,library(randomForest))


## Estimation function 
estimation <- function(count) {

  Data<-DataGen(N,seed*count)
  hopt <- 1.06*sd(Data$x)* N^{-1/5} 
  h <- hopt * N^{1/5} * N^{-2/7} 
  X<-Data$x
  Z<-Data$z
  D<-Data$d
  Y<-Data$y
  
  Naive <- mean(Y[D==1])-mean(Y[D==0])
  KIPW <- KSE_1(X,Y,D,Z,h)
  KREG <- KSE_3(X,Y,D,Z,h)
  KMR  <- KSE_t(X,Y,D,Z,hopt)
  # veff  <- estVeff(X,Y,D,Z,hopt)
  
  est <- cbind(Naive,KIPW,KREG,KMR)
  
  return(est)
}

est  <- parSapply(cl,1:J,estimation)
est  <- t(est)
colnames(est)<-c("Naive","KIPW","KREG","KMR")


# veff <- est[,5]
# est  <- est[,1:4]


result <- matrix(nrow = 4, ncol = 4)
colnames(result)<-c("bias","stdev","RMSE","CR")
rownames(result)<-c("Naive","KIPW","KREG","KMR")
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
  
  result[i,] <- cbind(bias,stdev,rmse,CR)
}

result


# # summary of veff
# mean(veff)
# sd(veff)
# mean(veff/sqrt(N))


# plot
est.df <- est[,-1]
plt_ATE(est.df)


# save(est,file="1.4.1 ResultN=500.RData")
# save(est,file="1.4.2 ResultN=1000.RData")


stopCluster(cl)



# # -------------boxplot----------------------
# method<-c("KIPW","KREG","KMR","Naive")
# est.df <- melt(est)[,-1]
# colnames(est.df) <- c("method","estimates")
# est.df$method = factor(est.df$method,levels = method)
# 
# # p_box <- ggplot(data=est.df) + geom_boxplot(mapping = aes(x=method,y=estimates),fill="gray") + theme_bw() + geom_hline(yintercept = truevalue)
# # p_box
# 
# boxplot(est.df$estimates~est.df$method,outline=F,xlab="",ylab="")
# abline(h=0.087)



