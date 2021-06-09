# kernel estimation
### not leave one out

K <- function(X,h){
  # kernel
  
  if (class(X)!="data.frame") { X = data.frame(X) }
  n = dim(X)[1]
  res = matrix(ncol = n, nrow = n)
  for (i in seq(n)) {
    ### continuous
    k_age        <- dnorm((X$age - X$age[i])/h)/h
    k_fatheduc   <- dnorm((X$fatheduc - X$fatheduc[i])/h)/h
    k_matheduc   <- dnorm((X$motheduc - X$motheduc[i])/h)/h
    k_iq         <- dnorm((X$iq - X$iq[i])/h)/h
    ### binary
    k_south66      <- X$south66==X$south66[i]
    k_black        <- X$black==X$black[i]
    k_smsa66       <- X$smsa66==X$smsa66[i]
    k_fatheduc.na  <- X$fatheduc.na==X$fatheduc.na[i]
    k_motheduc.na  <- X$motheduc.na==X$motheduc.na[i]
    k_iq.na        <- X$iq.na==X$iq.na[i]
    
    k = k_age*k_fatheduc*k_matheduc*k_iq*k_south66*k_black*k_smsa66*k_fatheduc.na*k_motheduc.na*k_iq.na
    res[,i] = k
  }
  return(res)
}


fhat <- function(X,Z,h,K_x){
  n = length(Z)
  res = vector(length = n)
  for (i in seq(n)) {
    nu <- sum(K_x[,i] * (Z==Z[i]))
    de <- sum(K_x[,i])
    res[i] = nu/de
  }
  return(res)
}

pDhat <- function(ind,X,Z,D,h,K_x){
  n = length(Z)
  res = vector(length = n)
  for (i in seq(n)) {
    nu <- sum(D * K_x[,i] * (Z==ind))
    de <- sum(K_x[,i] * (Z==ind))
    res[i] = nu/de
  }
  return(res)
}

pYhat <- function(ind,X,Z,Y,h,K_x){
  n = length(Z)
  res = vector(length = n)
  for (i in seq(n)) {
    nu <- sum(Y * K_x[,i] * (Z==ind))
    de <- sum(K_x[,i] * (Z==ind))
    res[i] = nu/de
  }
  return(res)
}

deltaDhat <- function(X,Z,D,h,K_x){
  return(pDhat(1,X,Z,D,h,K_x)-pDhat(0,X,Z,D,h,K_x))
}

deltaYhat <- function(X,Z,Y,h,K_x){
  return(pYhat(1,X,Z,Y,h,K_x)-pYhat(0,X,Z,Y,h,K_x))
}

# Estimators
KSE_0 <- function(X,Y,D,Z,h){
  n = length(Z)
  K_x = K(X,h)
  sum( (2*Z-1)/fhat(X,Z,h,K_x)*
             (pDhat(0,X,Z,D,h,K_x)* deltaYhat(X,Z,Y,h,K_x) / (deltaDhat(X,Z,D,h,K_x))^2 -
                pYhat(0,X,Z,Y,h,K_x) / deltaDhat(X,Z,D,h,K_x)) )/n
  }

KSE_1 <- function(X,Y,D,Z,h){
  n = length(Z)
  K_x = K(X,h)
  sum( (2*Z-1)/fhat(X,Z,h,K_x)*
             Y/deltaDhat(X,Z,D,h,K_x) ) /n
  }


KSE_2 <- function(X,Y,D,Z,h){
  n = length(Z)
  K_x = K(X,h)
  sum( (2*Z-1)/fhat(X,Z,h,K_x)*
             D*deltaYhat(X,Z,Y,h,K_x) / (deltaDhat(X,Z,D,h,K_x))^2 ) /n
  }

KSE_3 <- function(X,Y,D,Z,h){
  n = length(Z)
  K_x = K(X,h)
  sum( deltaYhat(X,Z,Y,h,K_x)/deltaDhat(X,Z,D,h,K_x) ) /n}

KSE_t <- function(X,Y,D,Z,h){
  n = length(Z)
  K_x = K(X,h)
  sum( (2*Z-1)/fhat(X,Z,h,K_x)*
         (pDhat(0,X,Z,D,h,K_x)* deltaYhat(X,Z,Y,h,K_x) / (deltaDhat(X,Z,D,h,K_x))^2 -
            pYhat(0,X,Z,Y,h,K_x) / deltaDhat(X,Z,D,h,K_x)) )/n+
    sum( (2*Z-1)/fhat(X,Z,h,K_x)*
           Y/deltaDhat(X,Z,D,h,K_x) ) /n-
    sum( (2*Z-1)/fhat(X,Z,h,K_x)*
           D*deltaYhat(X,Z,Y,h,K_x) / (deltaDhat(X,Z,D,h,K_x))^2 ) /n+
    sum( deltaYhat(X,Z,Y,h,K_x)/deltaDhat(X,Z,D,h,K_x) ) /n
  }

estVeff <- function(X,Y,D,Z,h){
  n = length(Z)
  phi_i =  (2*Z-1)/fhat(X,Z,h)*
         (Y/deltaDhat(X,Z,D,h) - D*deltaYhat(X,Z,Y,h) / (deltaDhat(X,Z,D,h))^2 +
            pDhat(0,X,Z,D,h)* deltaYhat(X,Z,Y,h) / (deltaDhat(X,Z,D,h))^2 -
            pYhat(0,X,Z,Y,h) / deltaDhat(X,Z,D,h)) +
         deltaYhat(X,Z,Y,h)/deltaDhat(X,Z,D,h) 
  sqrt(sum( (phi_i -  sum(phi_i)/n)^2  )/n )
}
