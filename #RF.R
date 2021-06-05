library(randomForest)
library(ggplot2)
library(dplyr)

source("Data_gen.R")
source("TrueValue.R")

N=2000
data <- DataGen(N,200)
x<-data$x;y<-data$y;z<-data$z;d<-data$d
data.df <- data.frame(x,y,z,d)
hopt <- 1.06*sqrt(var(x))* N^{-1/5}

ggplot(data = data.df, aes(x=x,y=z)) + geom_jitter()

#true
plot(x,expit(0.1-0.5*x))

# kernal estimation
fhat <- function(X,Z,h){
  n = length(Z)
  res = vector(length = n)
  for (i in seq(n)) {
    nu <- sum(dnorm((X-X[i])/h)/h * (Z==Z[i]))
    de <- sum(dnorm((X-X[i])/h)/h)
    res[i] = nu/de
  }
  return(res)
}
f <- fhat(x,z,hopt)

mutate(data.df,f=f) %>% filter(z==1) %>% 
ggplot(aes(x=x,y=f)) + geom_jitter()


# data.df.z1 <- filter(data.df,z==1)  
# data.df.z0 <- filter(data.df,z==0)  

# RF
yy <- expit(0.1-0.5*x)+rnorm(1000,sd=0.02)
plot(x, yy)
zofx <- randomForest(yy ~ x, data = data.frame(yy,x), maxnodes = 10)
xx = c(seq(-1, -0.5, length.out = 500),seq(0.5, 1, length.out = 500))
f2 <- predict(zofx, data.frame(x=xx))
ggplot(data.frame(xx,f2),aes(x=xx,y=f2)) + geom_point()



zofx <- randomForest(z ~ x, data = data.frame(z,x), maxnodes = 5)
f2 <- predict(zofx, x)


mutate(data.df,f=f2) %>%
  # filter(z==1) %>% 
  ggplot(aes(x=x,y=f)) + geom_point()


