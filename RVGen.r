
# Statistical computing
# Spring 2015
# Homework-4
#####################F(x)=x^3 and 0<x<1##################################

n<-1000
u<-runif(n)
x<-u^(1/3)
hist(x,prob=T,col="red",main=expression(f(x)==3*x^2))
###Alternatively:
hist(x,prob=T,col="red",main=bquote(f(x)==3*x^2))
y<-seq(0,1,0.01)
lines(y,3*y^2, lwd=5, col='blue')

##############################################################
#################Exponential distribution#####################
##############################################################
n<-100000
lambda=2
x<-(-log(runif(n))/lambda)

###################################################
#check out rexp####################################
###################################################

###################################################
#################TwoPointDist######################
###################################################
n<-1000
p<-0.4
u<-runif(n)
x<-as.integer(u>0.6)

mean(x)
var(x)

###alternative:
rbinom(n,size=1,prob=p)
sample(c(0,1),size=n,replace=T,prob=c(0.6,0.4))

###################################################
#################GeometricDist######################
###################################################

n<-1000
p<-0.25
u<-runif(n)
k<-ceiling(log(1-u)/log(1-p))-1
####simplification
k<-floor(log(u)/log(1-p))

###################################################
#################LogDist######################
###################################################
rlogarithmic<-function(n,theta){
	#returns a random logarithmic (theta) sample size n
	u<-runif(n)
	#set the initial length of cdf vector
	N<-ceiling(-16/log10(theta))
	k<-1:N
	a<- (-1/log(1-theta))
	fk<-exp(log(a)+k*log(theta)-log(k))
	FK<-cumsum(fk)
	x<-integer(n)
	for (i in 1:n){
		x[i]<-as.integer(sum(u[i]>FK))##F^{-1}(u)-1
		while(x[i]==N){
			#if x==N we need to extend the cdf
			#very unlikely because N is large
			logf<-log(a)+(N+1)*log(theta)-log(N+1)
			fk<-c(fk,exp(logf))
			FK<-c(FK,FK[N]+fk[N+1])
			N<-N+1
			x[i]<as.integer(sum(u[i]>FK))
		}
	}
	x+1
}



n<-1000
theta<-0.5
x<-rlogarithmic(n,theta)
#compute density of logarithmic (theta) for comparison
k<-sort(unique(x))
p<- -1/log(1-theta)*theta^k/k
se<-sqrt(p*(1-p)/n)
round(rbind(table(x)/n,p,se),3)

##################################################
#################Convultion and Mixture of RV######################
###################################################
n<-1000
x1<-rgamma(n,2,2)
x2<-rgamma(n,2,4)

s<-x1+x2 #the convultion

u<-runif(n)
k<-as.integer(u>0.5) #vectors of zeros and ones
x<-k*x1+(1-k)*x2 #the mixture

par(mfcol=c(1,2))
hist(s,prob=T,col="red")
hist(x,prob=T,col="blue")