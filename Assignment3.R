

# Statistical computing
# Spring 2015
# Homework-1

#*****************

library(expm) # important to calculate matrix to the power of n

# 2
#*****************
gambling<-function(){
  x<-rep(0,5)
  x[1]=2
  for (i in 2:5){
    if (x[i-1]==0 || x[i-1]==5){
      x[i]=x[i-1]}
      else{
        x[i]=x[i-1]+sample(c(1,-1),1,prob=c(0.6,0.4))
      }
   }
  return (x)
  }

prob_ending<-function(n){
  
F=replicate(n,gambling())

winning<- sum(F[5,]==0)
losing<- sum(F[5,]==5)

return((winning+losing)/n)
        
}



# 3
#*****************

P<-matrix(c(0.4,0.6,0.1,0.9),ncol=2,byrow=T) # transition matrix
S_0<-matrix(c(0,1),byrow=T,ncol=2)           #initial state

n<-1
while (n>0){                              # looping to get the steps at which it becomes stationary and the stationary matrix
  sx<-S_0%*%(P%^%n)
  sy<-S_0%*%(P%^%(n+1))
  n<-n+1
  if (sx==sy){
    break}
}
cat('stationarity is reached after',n, 'transions,\n')

print('the stationary distribution is:\n')

show(sx)
  
