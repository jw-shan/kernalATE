rm(list=ls())
library(reshape2)


# ----- N=500-----------

load("1.4.1 ResultN=500.RData")
load("1.4.2 CBE_N=500.RData")
load("1.4.3 TR_N=500.RData")

method<-c("KIPW","KREG","KMR","CBE",
          "MR-ALL","MR-M1","MR-M2",
          "BMR-ALL","BMR-M1","BMR-M2","BMR-M3",
          "Naive")
est.all <- cbind(est,est_CBE_500,est_tr_500)
est.all <- est.all[,method]
boxplot(est.all,outline=F)
abline(h=0.087)
points(colMeans(est.all), col = "red")



# ----- N=1000-----------
rm(list=ls())

load("1.5.1 ResultN=1000.RData")
load("1.5.2 CBE_N=1000.RData")
load("1.5.3 TR_N=1000.RData")


method<-c("KIPW","KREG","KMR","CBE",
          "MR-ALL","MR-M1","MR-M2",
          "BMR-ALL","BMR-M1","BMR-M2","BMR-M3",
          "Naive")

est.all <- cbind(est,est_CBE_1000,est_tr_1000)
est.all <- est.all[,method]
boxplot(est.all,outline=F)
abline(h=0.087)
points(colMeans(est.all), col = "red")
