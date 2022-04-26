library(ggplot2)
library(cowplot)


plt_ATE <- function(est){
  # est - N*4 data.frame
  
  p1 <- ggplot(est) +   geom_histogram(aes(x = est[,1])) + labs(x="KSE-1") + geom_vline(aes(xintercept=0.2866311), colour="red")
  p2 <- ggplot(est) +   geom_histogram(aes(x = est[,2])) + labs(x="KSE-2") + geom_vline(aes(xintercept=0.2866311), colour="red")
  p3 <- ggplot(est) +   geom_histogram(aes(x = est[,3])) + labs(x="KSE-3") + geom_vline(aes(xintercept=0.2866311), colour="red")
  p4 <- ggplot(est) +   geom_histogram(aes(x = est[,4])) + labs(x="KSE-T") + geom_vline(aes(xintercept=0.2866311), colour="red")
  plot_grid(p1, p2, p3, p4, nrow = 2)
  
  
}


