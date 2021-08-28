library(ggplot2)
library(cowplot)


plt_ATE <- function(est){
  # est - N*4 data.frame
  
  p1 <- ggplot(est) +   geom_histogram(aes(x = est[,1])) + labs(x="KIPW") + geom_vline(aes(xintercept=0.087), colour="red")+ theme_bw()
  # p2 <- ggplot(est) +   geom_histogram(aes(x = est[,2])) + labs(x="KSE-2") + geom_vline(aes(xintercept=0.087), colour="red")+ theme_bw()
  p3 <- ggplot(est) +   geom_histogram(aes(x = est[,2])) + labs(x="KREG") + geom_vline(aes(xintercept=0.087), colour="red")+ theme_bw()
  p4 <- ggplot(est) +   geom_histogram(aes(x = est[,3])) + labs(x="KMR") + geom_vline(aes(xintercept=0.087), colour="red")+ theme_bw()
  plot_grid(p1, p3, p4, nrow = 1)
  
  
}


