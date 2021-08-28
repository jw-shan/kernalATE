library(ggplot2)
library(cowplot)


plt_ATE <- function(est){
  # est - N*4 data.frame
  
  p1 <- ggplot(est) + geom_histogram(aes(x = est[,1]),bins=25,color="#666666",fill="white") + labs(x="KIPW") + geom_vline(aes(xintercept=0.087), colour="red",linetype=2,size=1.2)+ theme_classic() + scale_y_continuous(expand = c(0,0))
  p2 <- ggplot(est) + geom_histogram(aes(x = est[,2]),bins=25,color="#666666",fill="white") + labs(x="KREG") + geom_vline(aes(xintercept=0.087), colour="red",linetype=2,size=1.2)+ theme_classic() + scale_y_continuous(expand = c(0,0))
  p3 <- ggplot(est) + geom_histogram(aes(x = est[,3]),bins=25,color="#666666",fill="white") + labs(x="KMR")  + geom_vline(aes(xintercept=0.087), colour="red",linetype=2,size=1.2)+ theme_classic() + scale_y_continuous(expand = c(0,0))
  plot_grid(p1, p2, p3, nrow = 1)
  
  
}




# + geom_line(data=data.frame(x=seq(-4,4,0.05),y=dnorm(seq(-4,4,0.05))),mapping = aes(x,y),color="red")