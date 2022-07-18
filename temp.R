# test
Data = DataGen(N,4917)
X<-Data$x
Z<-Data$z
D<-Data$d
Y<-Data$y



# propensity
pix.true = Data$pix.true
f.true = Z*pix.true + (1-Z)*(1-pix.true)
f.est = fhat(X,Z,hopt)
plot(f.true,f.est)
# CV
fhat_bw = npregbw(Z~X)
pi.hat.cv = fitted(npreg(fhat_bw,exdat=X))
f.est.cv = Z*pi.hat.cv + (1-Z)*(1-pi.hat.cv)
plot(f.true,f.est.cv)


# pD
pD.true = Data$p0p1.d.true

pD0.est = pDhat(0,X,Z,D,hopt)
plot(pD.true[1,],pD0.est)
abline(a=0,b=1)

pD1.est = pDhat(1,X,Z,D,hopt)
plot(pD.true[2,],pD1.est)
abline(a=0,b=1)

pD0.hat_bw = npregbw(D~X,subset = (Z==0))
pD0.cv = fitted(npreg(pD0.hat_bw,exdat=X))
plot(pD.true[1,],pD0.cv)
abline(a=0,b=1)

pD1.hat_bw = npregbw(D~X,subset = (Z==1))
pD1.cv = fitted(npreg(pD1.hat_bw,exdat=X))
plot(pD.true[2,],pD1.cv)
abline(a=0,b=1)