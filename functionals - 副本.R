# kernal estimation


fhat <- function(x,z,X,Z,h){
  nu <- sum(dnorm((X-x)/h)/h * (Z==z))
  de <- sum(dnorm((X-x)/h)/h)
  return(nu/de)
}

pDhat <- function(x,z,X,Z,D,h){
  nu <- sum(D * dnorm((X-x)/h)/h * (Z==z))
  de <- sum(dnorm((X-x)/h)/h * (Z==z))
  return(nu/de)
}

pYhat <- function(x,z,X,Z,Y,h){
  nu <- sum(Y * dnorm((X-x)/h)/h * (Z==z))
  de <- sum(dnorm((X-x)/h)/h * (Z==z))
  return(nu/de)
}