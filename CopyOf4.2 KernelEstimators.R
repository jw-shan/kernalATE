# kernel estimation
### not leave one out

K <- function(X,h){
  # kernel
  
  if (class(X)!="data.frame") { X = data.frame(X) }
  n = dim(X)[1]
  res = matrix(ncol = n, nrow = n)
  for (i in seq(n)) {
    ### continuous
    k_age         <- dnorm((X$age - X$age[i])/h)/h
    k_parentseduc <- dnorm((X$parentseduc - X$parentseduc[i])/h)/h
    k_iq          <- dnorm((X$iq - X$iq[i])/h)/h
    ### binary
    k_south66      <- X$south66==X$south66[i]
    k_black        <- X$black==X$black[i]
    k_smsa66       <- X$smsa66==X$smsa66[i]

    k = k_age*k_parentseduc*k_iq*k_south66*k_black*k_smsa66
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
  res[which(res==0)]=0.0001
  res[which(res==1)]=1-0.0001
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
  res = pDhat(1,X,Z,D,h,K_x)-pDhat(0,X,Z,D,h,K_x)
  res[which(res==0)]=0.001
  return(res)
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
  # sum( (2*Z-1)/fhat(X,Z,h,K_x)*
  #        (pDhat(0,X,Z,D,h,K_x)* deltaYhat(X,Z,Y,h,K_x) / (deltaDhat(X,Z,D,h,K_x))^2 -
  #           pYhat(0,X,Z,Y,h,K_x) / deltaDhat(X,Z,D,h,K_x)) )/n+
  #   sum( (2*Z-1)/fhat(X,Z,h,K_x)*
  #          Y/deltaDhat(X,Z,D,h,K_x) ) /n-
  #   sum( (2*Z-1)/fhat(X,Z,h,K_x)*
  #          D*deltaYhat(X,Z,Y,h,K_x) / (deltaDhat(X,Z,D,h,K_x))^2 ) /n+
  #   sum( deltaYhat(X,Z,Y,h,K_x)/deltaDhat(X,Z,D,h,K_x) ) /n
  phi_i =  (2*Z-1)/fhat(X,Z,h,K_x)*
    (Y/deltaDhat(X,Z,D,h,K_x) - D*deltaYhat(X,Z,Y,h,K_x) / (deltaDhat(X,Z,D,h,K_x))^2 +
       pDhat(0,X,Z,D,h,K_x)* deltaYhat(X,Z,Y,h,K_x) / (deltaDhat(X,Z,D,h,K_x))^2 -
       pYhat(0,X,Z,Y,h,K_x) / deltaDhat(X,Z,D,h,K_x)) +
    deltaYhat(X,Z,Y,h,K_x)/deltaDhat(X,Z,D,h,K_x) 
  return( list(phi=phi_i,est=mean(phi_i),estVeff=sqrt(mean((phi_i-sum(phi_i)/n)^2))) )
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
