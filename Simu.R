source("functionals.R")
source("TrueValue.R")
source("Data_gen.R")


truevalue<- 0.087
## Monto Carlo times and Sample Size###
J <- 500
N <- 1000

## Bandwidth
h <- N^{-5/16}
hopt <- N^{-1/5} #optimal bandwidth of univariate

## Initialization
count <- 0
# T0est <- vector()
T1est <- vector()
T2est <- vector()
T3est <- vector()
Test  <- vector()

## Estimation
repeat {
  if (count == J) break
  print(count)
  count <- count + 1
  
  Data<-DataGen(N,100+count)
  X<-Data$x
  Z<-Data$z
  D<-Data$d
  Y<-Data$y
  
  # T0est[count] <- T0(X,Y,D,Z,h)
  #T1est[count] <- T1(X,Y,D,Z,h)
  #T2est[count] <- T2(X,Y,D,Z,h)
  #T3est[count] <- T3(X,Y,D,Z,h)
  #print(T1est[count])
  #print(T2est[count])
  #print(T3est[count])
  Test[count] <- Tt(X,Y,D,Z,hopt)
  print(Test[count])
}