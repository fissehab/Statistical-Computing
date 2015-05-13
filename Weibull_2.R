# Examples on Weibull distribution

par(mfrow=c(3,2))

high=qweibull(.99,shape=0.5,scale=10)

curve(dweibull(x, scale=10,shape=0.5),from=0,to=high,lwd=3,col="blue",main="high number of initial failures")

high=qweibull(.99,shape=1,scale=10)
curve(dweibull(x, scale=10,shape=1),from=0,to=high,lwd=3,col="blue",main="random and multiple cause failure")

high=qweibull(.99,shape=1.5,scale=10)
curve(dweibull(x, scale=10,shape=1.5),from=0,to=high,lwd=3,col="blue",main="early wear-out failures")

high=qweibull(.99,shape=2,scale=10)
curve(dweibull(x, scale=10,shape=2),from=0,to=high,lwd=3,col="blue",main="linearly increasing failure rare")

high=qweibull(.99,shape=3.5,scale=10)
curve(dweibull(x, scale=10,shape=3.5),from=0,to=high,lwd=3,col="blue",main="rapid wear-out faily during final period of prod life")

high=qweibull(.99,shape=20,scale=10)
curve(dweibull(x, scale=10,shape=20),from=0,to=high,lwd=3,col="blue",main="similar to extreme value dist.")
