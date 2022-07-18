rm(list=ls())
library(parallel)
library(reshape2)
library(np)


source("1.1 Datagen_1D.R")
source("1.2 KernelEstimators.R")
source("1.2.2 MRCV.R")
source("1.3 plot.R")


## Monto Carlo times and Sample Size###
seed = 11
J <- 1000
N <- 5000
truevalue<- 0.087

# ## Monto Carlo times and Sample Size###
# seed = 11
# J <- 500
# N <- 1000
# truevalue<- 0.087





# 
# # test
# Data = DataGen(N,4917)
# X<-Data$x
# Z<-Data$z
# D<-Data$d
# Y<-Data$y
# hopt <- 1.06*sd(Data$x)* N^{-1/5}
# h <- hopt * N^{1/5} * N^{-2/7}
# 
# 
# # propensity
# pix.true = Data$pix.true
# f.true = Z*pix.true + (1-Z)*(1-pix.true)
# f.est = fhat(X,Z,hopt)
# plot(f.true,f.est)
# # CV
# fhat_bw = npregbw(Z~X)
# pi.hat.cv = fitted(npreg(fhat_bw,exdat=X))
# f.est.cv = Z*pi.hat.cv + (1-Z)*(1-pi.hat.cv)
# plot(f.true,f.est.cv)
# 
# 
# # pD
# pD.true = Data$p0p1.d.true
# 
# pD0.est = pDhat(0,X,Z,D,hopt)
# plot(pD.true[1,],pD0.est)
# abline(a=0,b=1)
# 
# pD1.est = pDhat(1,X,Z,D,hopt)
# plot(pD.true[2,],pD1.est)
# abline(a=0,b=1)
# 
# pD0.hat_bw = npregbw(D~X,subset = (Z==0))
# pD0.cv = fitted(npreg(pD0.hat_bw,exdat=X))
# plot(pD.true[1,],pD0.cv)
# abline(a=0,b=1)
# 
# pD1.hat_bw = npregbw(D~X,subset = (Z==1))
# pD1.cv = fitted(npreg(pD1.hat_bw,exdat=X))
# plot(pD.true[2,],pD1.cv)
# abline(a=0,b=1)























# parallel setting
# configure parallel environment
ncores = detectCores()
if (ncores<40) {
  cl = makeCluster(ncores)
}else{
  cl = makeCluster(40)
}
clusterExport(cl,ls())
clusterEvalQ(cl,library(np))


## Estimation function 
estimation <- function(count) {
  
  Data<-DataGen(N,seed*count)
  X<-Data$x
  Z<-Data$z
  D<-Data$d
  Y<-Data$y
  # hopt <- 1.06*sd(Data$x)* N^{-1/5}
  # h <- hopt * N^{1/5} * N^{-2/7}
  
  
  
  est.list  <- KSE_CV(X,Y,D,Z)
  # veff  <- estVeff(X,Y,D,Z,hopt)
  
  IPW = est.list[["IPW"]]
  REG = est.list[["REG"]]
  MR  = est.list[["MR"]]
  est <- cbind(IPW,REG,MR)
  
  return(est)
}

est  <- parSapply(cl,1:J,estimation)
est  <- t(est)
colnames(est)<-c("IPW","REG","MR")

# colnames(est)<-c("Naive","KIPW","KREG","KMR")


# veff <- est[,5]
# est  <- est[,1:4]


result <- matrix(nrow = 3, ncol = 4)
colnames(result)<-c("bias","stdev","RMSE","CR")
rownames(result)<-c("IPW","REG","MR")
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
est.df <- data.frame(est)
plt_ATE(est.df)

est.norm.df <- data.frame(sqrt(N)*(est-truevalue))
plt_ATE(est.norm.df)

# save(est,file="1.4.1 ResultN=500.RData")
# save(est,file="1.4.2 ResultN=1000.RData")


stopCluster(cl)



# -------------boxplot----------------------
method<-c("KIPW","KREG","KMR","Naive")
est.df <- melt(est)[,-1]
colnames(est.df) <- c("method","estimates")
est.df$method = factor(est.df$method,levels = method)

# p_box <- ggplot(data=est.df) + geom_boxplot(mapping = aes(x=method,y=estimates),fill="gray") + theme_bw() + geom_hline(yintercept = truevalue)
# p_box

boxplot(est.df$estimates~est.df$method,outline=F,xlab="",ylab="")
abline(h=0.087)



