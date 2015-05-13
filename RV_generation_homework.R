
# Statistical computing
# Spring 2015
# Homework-5

# Randon number generation using inverse method

# f(x)=3x^2,(0,1) estimate a random sample

n=100000
u=runif(n)

x=u^(1/3)

hist(x,prob=T, col="red",main=expression(f(x)==3*x^2))


# Alternatively

hist(x,prob=T,col="red",main=bquote(f(x)==3*x^2))

y<-seq(0,1,0.01)
lines(y,3*y^2, lwd=5, col='blue')


# Generate a random sample from the exponential with mean 1/lambda
# assume lambda=10

# check the mean and variance with the theorettical expectation and variance of 1/lambda and 1/lambda^2

# cdf(x) =1-exp(-lambda*x)

expo1<-function(lambda){
  
  n=1000000
  u=runif(n)
  
  x=-log(1-u)/lambda
  
  return (hist(x,prob=T, col="red",main=expression(f(x)==lambda*exp^(-lambda*x))))
  
}


# bernoulli inversion
# algorithm
# generate U from runif(0,1)
# If U le p, then X=1; else X=0

# check with the theoretical mean and variance, p, pq, respectively

n=1000000
x=seq(0,0,len=n)
p=0.4

for (k in 1:n){
  if (runif(1) <=p){
    x[k]=1}
  else {x[k]=0}}

hist(x,prob=FALSE,col="light blue", main='Bernoulli, p=0.4')

table(x)/n



# simulate a Logarithmic(theta) random sample by the inverse transform method

rloga<-function(n, theta){
  
     # n is number of realizations
  
  u<-runif(n)   # generate logarithmic sample
  
  v<-runif(n)
  
  x<-floor(1+log(v)/log(1-(1-theta)^u))
  
  return( hist(x,prob=FALSE,col="light blue", main='logarithmic'))
}

# example

rloga(10000,0.4)

# On average how many r.v. must be simulated to generate 1000 variates 
# from the Beta (??=2,??=2) distribution by acceptance rejection method?

# Acceptance-rejection method for Beta(2,2)

# f(x) = gamma(2+2)/(gamma(2)*gamma(2))*x^(2-1)*(1-x)^(2-1)
# f(x) = 6x(1-x)  ; 0<x<1

accepReject<-function(n=1000) #default to generate 1000 random numbers
  {    
x <- NULL      # to store the generated numbers
i <- 0         # counter for accepted
j <- 0          # a counter to store the total number of iterations
while(i<=n ){
  j <- j + 1
  y <- runif(1)
  u <- runif(1)
  if(u < 1.5*y*(1-y)){
    x <- c(x,y)       # store y to x
    i <- i + 1
  }
}
return (j)
}





n <- 1000      # to generate 1000 random numbers
x <- NULL      # to store the generated numbers
i <- 0         # counter for accepted
j <- 0          # a counter to store the total number of iterations
while(i<=n ){
  j <- j + 1
  y <- runif(1)
  u <- runif(1)
  if(u < 1.5*y*(1-y)){
    x <- c(x,y)       # store y to x
    i <- i + 1
  }
}

hist(x, prob=TRUE, main=expression(f(x)==6*x(1-x)))
y <- seq(0,1,.001)
lines(y, 6*y*(1-y))

# In the above solution, we took c = 6, so on average 6000 iterations
# are required to generate a sample of size n = 1000.



# the above algarithm can be made better as shown below
# using the fact that x(1-x) in [0,1] is atmost 1/4, c=6/4=1.5


n <- 1000 # to generate 1000 random numbers
x <- NULL # to store the generated numbers
i <- 0 # counter
j <- 0 # a counter to store the total number of iterations
while(i<=n){
  j <- j + 1
  y <- runif(1)
  u <- runif(1)
  if(u<1.5 * y*(1-y)){
    x <- c(x,y) # store y to x
    i <- i + 1
  }
}
cat("\ntotal number of iteration:=", j,"\n")

# The QQ plot can be calculated as below; which is nearly normal as it should be

q<-qbeta(ppoints(n),2,2)
qqplot(q,x,cex=0.25,xlab="Beta(2,2)",ylab="Sample")
abline(0,1)
