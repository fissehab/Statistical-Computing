# Suppose that an orange juice (OJ) company (call it brand A) controlls 20% of the OJ market. Suppose they hire a market research company to 
# predict the effects of an aggressive add campaign. Suppose they conclude:
#   someone using brand A will stay with brand A with 90% probability
# someone using other brands will switch to brand A with 70% probabilility
# Assume OJ is bought weekly
# What is the probability of someone busying brand A after one week, two weeks, three weeks, n weeks

# Initial state distribution matrix, S_0 =[0.2,0.8]; this is [A,A']
# transition probability matrix P=matrix(c(0.9,0.1,0.7,0.3),ncol=2,byrow=T); column(current state); row(next state)

#probability of someone buying brand A after n weeks is
#s_n=s_0*p^n


library(expm) # important to calculate matrix to the power of n
library(MASS)


#week_1

# to calculate at the nth week

fish<-function(n){ 
  P<-matrix(c(0.9,0.1,0.7,0.3),ncol=2,byrow=T)
  S_0<-matrix(c(0.2,0.8),byrow=T,ncol=2)
  S<-1*S_0
  
  for (i in 1:n){
    
  S<-S%*%P
  
  }
  S
}


# 

fish1<-function(n){ 
  P<-matrix(c(0.9,0.1,0.7,0.3),ncol=2,byrow=T)
  S_0<-matrix(c(0.2,0.8),byrow=T,ncol=2)
  S<-S_0%*%(P%^%n)
  S
}

##

fish<-function(n){ 
  P<-matrix(c(0.8,0.2,0.3,0.7),ncol=4,byrow=T)
  S_0<-matrix(c(1,0,0,0,0.1,0.8,0.1,0,0.1,0.4,0.4,0.1,0,0,0,1),byrow=T,ncol=4)
  S<-1*S_0
  for (i in 1:n){
    S<-S%*%S}
  S}

#
fish<-function(n){ 
  P<-matrix(c(0.5,0.5,0.5,0.5),ncol=2,byrow=T)
  S_0<-matrix(c(0,1),byrow=T,ncol=2)
  S<-1*S_0
  for (i in 1:n){
    S<-S%*%P}
  S}




example<-function(n){ 
  P<-matrix(c(0.8,0.2,0.5,0.5),ncol=2,byrow=T)
  S_0<-matrix(c(1,0),byrow=T,ncol=2)
  S<-S_0%*%(P%^%n)
  
  S_0<-matrix(c(0,1),byrow=T,ncol=2)
  S2<-S_0%*%(P%^%n)
  matrix(cbind(S[1],S2[1]),ncol=1)
}

example<-function(n){ 
  P<-matrix(c(0.4,0.5,0.1,0.3,0.3,0.4,0.1,0.7,0.2),ncol=3,byrow=T)
  S_0<-matrix(c(0.1,0.1,0.8),byrow=T,ncol=3)
  S<-S_0%*%(P%^%n)
  S
}

#

example<-function(n){ 
  P<-matrix(c(0.5,0.4,0.1,0.1,0.6,0.3,0.1,0.2,0.7),ncol=3,byrow=T)
  S_0<-matrix(c(0.1,0.1,0.8),byrow=T,ncol=3)
  S<-S_0%*%(P%^%n)
  S
}


fish1<-function(n){ 
  P<-matrix(c(0.4,0.6,0.1,0.9),ncol=2,byrow=T)
  S_0<-matrix(c(0,1),byrow=T,ncol=2)
  S<-S_0%*%(P%^%n)
  S
 }



example<-function(n){ 
  P<-matrix(c(1./3,1./3,1./3,0,0,0,1./2,1./2,0,1,0,0,1./2,0,0,1./2),ncol=4,byrow=T)
  S_0<-matrix(c(1./4,1./4,1./4,1./4),byrow=T,ncol=4)
  S<-S_0%*%(P%^%n)
  fractions(S)
}


gambler<-function(p,n,N, nsim){
  # p is probability of winning
  # n is initial money he has
  # nsim is number of simulations
  # total money available

x<-rep(0,nsim)
x[1]=n



for (i in 2:nsim){
  if (x[i-1]==0||x[i-1]==N){
    x[i]<-x[i-1]
}
else{
  x[i]<-x[i-1]+sample(c(1,-1),1,prob=c(p,1-p))
}
  
  plot(x,type='l',lwd=2,ylim=c(0,N),col='dark blue',xlab='bet')
}
}



# Example 


example<-function(n){ 
      x[1]=1
  P<-matrix(c(0.5,0.5,0,0, 0.7,0.1,0.2,0,0,0.1,0.1,0.8,0,0,0.7,0.3),ncol=4,byrow=T)
  for (i in 2:n){
  x[i]<-x[i-1]+sample(c(3,2,1,0),4,prob=c(p,1-p))
  
  }
  plot(x,type='l',lwd=4,ylim=c(0,N))
  
}

if ps(i-1)==1
 z=runif(1,0,1)
if 




# commuters

transit<-function(n){ 
  P<-matrix(c(0.8,0.2,0.3,0.7),ncol=2,byrow=T)
  S_0<-matrix(c(0.25,0.75),byrow=T,ncol=2)
  S<-S_0%*%(P%^%n)
  S
}

# to compute the stationary matrix of the cummuters above (that is
# the percentage use in the long run)


stationary<-function(p=matrix(c(0.8,0.2,0.3,0.7),ncol=2,byrow=T),s_0=matrix(c(0.25,0.75),byrow=T,ncol=2)){ 
S1 <-s_0 %*%p
i<-1

while (i>=1){
  
  S2<-S1%*%p
 
  if (round(max(abs(S2-S1)),digits=8)==0){
    i<-0
  }

else {S1<-S2}}
   S2 
}





