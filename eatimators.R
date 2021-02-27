# kernal estimations
source(functions.R)


# data: X,Y,Z,D


n <- length(Y) 
h <- n^(-5/16)


# estimators
T0 <- sum( (2*Z-1)/fhat(X,Z,h)*
            (pDhat(0,X,Z,D,h)*
             deltaYhat(X,Z,Y,h) / (deltaDhat(X,Z,D,h))^2 -
            pYhat(0,X,Z,Y,h) / deltaDhat(X,Z,D,h))
           )/n

T1 <- sum( (2*Z-1)/fhat(X,Z,h)*
             Y/deltaDhat(X,Z,D,h) ) /n

T2 <- sum( (2*Z-1)/fhat(X,Z,h)*
             D*deltaYhat(X,Z,Y,h) / (deltaDhat(X,Z,D,h))^2 ) /n

T3 <- sum( deltaYhat(X,Z,Y,h)/deltaDhat(X,Z,D,h) ) /n

T <- T0 + T1 - T2 + T2