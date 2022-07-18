library(ggplot2)
library(cowplot)


plt_ATE <- function(est){
  # est - N*3 data.frame
  
  est1 <- est[,1]; est1 <- est1[truevalue-2<est1&est1<truevalue+2]; est1 <- data.frame(est1=est1)
  est2 <- est[,2]; est2 <- est2[truevalue-2<est2&est2<truevalue+2]; est2 <- data.frame(est2=est2)
  est3 <- est[,3]; est3 <- est3[truevalue-2<est3&est3<truevalue+2]; est3 <- data.frame(est3=est3)
  
  
  p1 <- ggplot(est1) + geom_histogram(aes(x = est1),bins=25,color="#666666",fill="white") + labs(x="KIPW") + geom_vline(aes(xintercept=0.087), colour="red",linetype=2,size=1.2)+ theme_classic() + scale_y_continuous(expand = c(0,0))
  p2 <- ggplot(est2) + geom_histogram(aes(x = est2),bins=25,color="#666666",fill="white") + labs(x="KREG") + geom_vline(aes(xintercept=0.087), colour="red",linetype=2,size=1.2)+ theme_classic() + scale_y_continuous(expand = c(0,0))
  p3 <- ggplot(est3) + geom_histogram(aes(x = est3),bins=25,color="#666666",fill="white") + labs(x="KMR")  + geom_vline(aes(xintercept=0.087), colour="red",linetype=2,size=1.2)+ theme_classic() + scale_y_continuous(expand = c(0,0))
  plot_grid(p1, p2, p3, nrow = 1)
  
  
}

x=1;x=2

# [est[,1]<truevalue+1&est[,1]>truevalue-1,1]


# + geom_line(data=data.frame(x=seq(-4,4,0.05),y=dnorm(seq(-4,4,0.05))),mapping = aes(x,y),color="red")