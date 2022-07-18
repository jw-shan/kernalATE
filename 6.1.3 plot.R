library(ggplot2)
library(cowplot)


plt_ATE <- function(est){
  # est - N*3 data.frame
  
  # est1 <- est[,1]-truevalue; est1 <- est1[truevalue-2<est1&est1<truevalue+2]; est1 <- data.frame(est1=est1)
  # est2 <- est[,2]-truevalue; est2 <- est2[truevalue-2<est2&est2<truevalue+2]; est2 <- data.frame(est2=est2)
  # est3 <- est[,3]-truevalue; est3 <- est3[truevalue-2<est3&est3<truevalue+2]; est3 <- data.frame(est3=est3)

  
  # normalization
  for (i in 1:3) {
    sdd=sd(est[which(truevalue-3<est[,i]&est[,i]<truevalue+3),i])
    est[,i] = est[,i]/sdd
  }
  
  
  
  x = seq(-6,6,0.01)
  norm = data.frame(x=x,y=dnorm(x))
  # norm = data.frame(x=x,
  #                   y1=dnorm(x,sd=sd(est[which(truevalue-2<est[,1]&est[,1]<truevalue+2),1])),
  #                   y2=dnorm(x,sd=sd(est[which(truevalue-2<est[,2]&est[,2]<truevalue+2),2])),
  #                   y3=dnorm(x,sd=sd(est[which(truevalue-2<est[,3]&est[,3]<truevalue+2),3])))
  
  p1 <- ggplot() + geom_histogram(data=est,mapping=aes(x = KIPW,after_stat(density)),bins=200,color="#666666",fill="white") + labs(x="KIPW") + geom_line(data=norm,aes(x=x,y=y),colour="red") +  theme_classic() + scale_y_continuous(expand = c(0,0)) + xlim(-6,6)
  p2 <- ggplot() + geom_histogram(data=est,mapping=aes(x = KREG,after_stat(density)),bins=200,color="#666666",fill="white") + labs(x="KREG") + geom_line(data=norm,aes(x=x,y=y),colour="red") +  theme_classic() + scale_y_continuous(expand = c(0,0))+ xlim(-6,6)
  p3 <- ggplot() + geom_histogram(data=est,mapping=aes(x = KMR,after_stat(density)),bins=200,color="#666666",fill="white") + labs(x="KMR")  + geom_line(data=norm,aes(x=x,y=y),colour="red") +  theme_classic() + scale_y_continuous(expand = c(0,0))+ xlim(-6,6)
  plot_grid(p1, p2, p3, nrow = 1)
  
}



# [est[,1]<truevalue+1&est[,1]>truevalue-1,1]


# + geom_line(data=data.frame(x=seq(-4,4,0.05),y=dnorm(seq(-4,4,0.05))),mapping = aes(x,y),color="red")
# geom_vline(aes(xintercept=0), colour="red",linetype=1,size=0.3)+