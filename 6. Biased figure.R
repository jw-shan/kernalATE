

# x=seq(-2,2,0.02)
x = c(seq(-1,-0.5,0.001),seq(0.5,1,0.001))
n = length(x)
y=0.15*sin(20*x)+ 0.1*sin(15*x) +  0.5 
# + rnorm(n,sd=0.03)
plot(x,y)

z = rbinom(n,1,y)

hist(rnorm(100))


library(randomForest)

fit1 = randomForest(z~x,nodesize = 100)
yhat = predict(fit1,x)
plot(x,yhat)
plot(y,yhat)
abline(a=0,b=1,col="red")


library(np)

bws = npregbw(z~x)
pi.hat = fitted(npreg(tydat=z ,txdat=x, exdat=x, bws=bws))
plot(x,pi.hat)
plot(y,pi.hat)
abline(a=0,b=1,col="red")
