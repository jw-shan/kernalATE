rm(list=ls())
library(parallel)
library(reshape2)

source("6.1.1 Datagen.R")
source("6.1.2 estimator.R")
source("6.1.3 plot.R")


## Monto Carlo times and Sample Size###
seed = 220704
J <- 2000
N <- 1000
# truevalue<- 0.087
truevalue <- 0.434

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
clusterEvalQ(cl,c(library(np),library(randomForest)))


## Estimation function 
estimation <- function(count) {

  Data<-DataGen(N,seed*count)

  X<-Data$x
  Z<-Data$z
  D<-Data$d
  Y<-Data$y
  
  est = KSE_CV(X,Y,D,Z)
  # est = RF(X,Y,D,Z)
  est <- cbind(est[[1]],est[[2]],est[[3]])
  
  return(est)
}

est  <- parSapply(cl,1:J,estimation)
est  <- t(est)
colnames(est)<-c("KIPW","KREG","KMR")


# veff <- est[,5]
# est  <- est[,1:4]


result <- matrix(nrow = 3, ncol = 4)
colnames(result)<-c("bias","stdev","RMSE","CR")
rownames(result)<-c("KIPW","KREG","KMR")
for (i in 1:3) {
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
# est.norm <- t(t(est - truevalue)/apply(est,2,sd))
est.df <- data.frame(est-truevalue)
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



