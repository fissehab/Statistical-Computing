

# Statistical computing
# Spring 2015
# Homework-1

#*****************
# 1
#*****************

rm(list=ls())

#input
x.values<-seq(-2,2,by=0.1)

#for each x calculate y

n=length(x.values)

y.values<-rep(0,n)

for (i in 1:n){
   
 if (x.values[i]>1)  {y.values[i]<-sqrt(x.values[i])}

 if (x.values[i]> 0 & x.values[i]<=1){y.values[i]<-x.values[i]^2 }

 if (x.values[i]<=0){y.values[i]<--1*x.values[i]^3 }
}


#output
 plot (x.values, y.values, type='l')


#*****************
# 2
#*****************

# a for loop to calculate a geometric series

rm(list=ls())

cat("provide x and n to calculate 1+x+x^2+...+x^n")

x <- as.numeric(readline("give the value of x: "))

n <- as.numeric(readline("give the value of n: "))

y=0

for (i in 0:n) {y=y+x^i}
print('the sum of the geometric series is:\n')

show(y)


#*****************
# 3
#*****************
 # question 2 in function form

geometric<-function(x,n){y=0
for (i in 0:n){
y=y+x^i}
y
}

#*****************
# 4
#*****************

geometric2<-function(x,n)
  {y<-(1-x^(n+1))/(1-x)
y
}

#*****************
# 5
#*****************

#5.a the same as question 2 above but using while loop instead of for loop


rm(list=ls())

cat("provide x and n to calculate 1+x+x^2+...+x^n")

x <- as.numeric(readline("give the value of x: "))

n <- as.numeric(readline("give the value of n: "))

y<-0
count<-0;

while (count <=n) {
  y<-y+x^count
  count<-count+1}

print('the sum of the geometric series is:\n')

show(y)



#function form of 5.a

rm(list=ls())

geometric2<-function(x,n){
 y<-0
 count<-0
 while (count<=n){
   y<-y+x^count
   count<-count+1
}
y

}

#5.vector form of 5.a.

rm(list=ls())

geometric_vector<-function(x,n){
  sum((x)^(c(0:n)))

}

#*****************
# 6
#*****************

 # finds the minimum of a vectro x


minimum <-function(x){
  y <-x[1]   ## set target to first element of `x`
  
  for (i in 1:length(x))
  {
    if (x[i]< y){y<-x[i]} 
  }
  
  y
  
}





#*****************
# 7
#*****************
# Suppose that (x(t),y(t)) has polar coordinates 
# Plot (x(t),y(y)) for t in [0,10].

t <- 0:10
x <- sqrt(t)
y <- 2*pi*t

xtext<-expression(paste("x = ",sqrt(t)))
ytext<-expression(paste("y = ",2*pi*t))

plot(x,y,type="l",xlab=xtext,ylab=ytext)





