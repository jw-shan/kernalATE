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

pDhat <- function(ind,X,Z,D,h){
  n = length(Z)
  res = vector(length = n)
  for (i in seq(n)) {
    nu <- sum(D * dnorm((X-X[i])/h)/h * (Z==ind))
    de <- sum(dnorm((X-X[i])/h)/h * (Z==ind))
    res[i] = nu/de
  }
  return(res)
}

pYhat <- function(ind,X,Z,Y,h){
  n = length(Z)
  res = vector(length = n)
  for (i in seq(n)) {
    nu <- sum(Y * dnorm((X-X[i])/h)/h * (Z==ind))
    de <- sum(dnorm((X-X[i])/h)/h * (Z==ind))
    res[i] = nu/de
  }
  return(res)
}

deltaDhat <- function(X,Z,D,h){
  return(pDhat(1,X,Z,D,h)-pDhat(0,X,Z,D,h))
}

deltaYhat <- function(X,Z,Y,h){
  return(pYhat(1,X,Z,Y,h)-pYhat(0,X,Z,Y,h))
}

# Estimators
T0 <- function(X,Y,D,Z,h){
  n = length(Z)
  sum( (2*Z-1)/fhat(X,Z,h)*
             (pDhat(0,X,Z,D,h)* deltaYhat(X,Z,Y,h) / (deltaDhat(X,Z,D,h))^2 -
                pYhat(0,X,Z,Y,h) / deltaDhat(X,Z,D,h)) )/n
  }

T1 <- function(X,Y,D,Z,h){
  n = length(Z)
  sum( (2*Z-1)/fhat(X,Z,h)*
             Y/deltaDhat(X,Z,D,h) ) /n
  }


T2 <- function(X,Y,D,Z,h){
  n = length(Z)
  sum( (2*Z-1)/fhat(X,Z,h)*
             D*deltaYhat(X,Z,Y,h) / (deltaDhat(X,Z,D,h))^2 ) /n
  }

T3 <- function(X,Y,D,Z,h){
  n = length(Z)
  sum( deltaYhat(X,Z,Y,h)/deltaDhat(X,Z,D,h) ) /n}

Tt <- function(X,Y,D,Z,h){T0(X,Y,D,Z,h) + T1(X,Y,D,Z,h) - T2(X,Y,D,Z,h) + T3(X,Y,D,Z,h)}

estVeff <- function(X,Y,D,Z,h){
  n = length(Z)
  phi_i =  (2*Z-1)/fhat(X,Z,h)*
         (Y/deltaDhat(X,Z,D,h) - D*deltaYhat(X,Z,Y,h) / (deltaDhat(X,Z,D,h))^2 +
            pDhat(0,X,Z,D,h)* deltaYhat(X,Z,Y,h) / (deltaDhat(X,Z,D,h))^2 -
            pYhat(0,X,Z,Y,h) / deltaDhat(X,Z,D,h)) +
         deltaYhat(X,Z,Y,h)/deltaDhat(X,Z,D,h) 
  sqrt(sum( (phi_i -  sum(phi_i)/n)^2  )/n )
}
