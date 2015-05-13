
# Statistical computing
# Spring 2015
# Weibull

x<-rlnorm(10000)
alpha=10
par(mfrow=c(3,2))
curve(dweibull(x, scale=10^(-1/0.5),shape=0.5),lwd=3,col="red",main="high number of initial failures")
curve(dweibull(x, scale=10^(-1),shape=1),lwd=3,col="red",main="random and multiple cause failure")
curve(dweibull(x, scale=10^(-1/1.5),shape=1.5),lwd=3,col="red",main="early wear-out failures")
curve(dweibull(x, scale=10^(-1/2),shape=2),lwd=3,col="red",main="linearly increasing failure case")
curve(dweibull(x, scale=10^(-1/3.5),shape=3.5),lwd=3,col="red",main="rapid wear-out failure during final period of production life")
curve(dweibull(x, scale=10^(-1/20),shape=20),lwd=3,col="red",main="similar to extreme value dististribution")