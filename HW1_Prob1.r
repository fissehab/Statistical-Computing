# HW1_Prob1
# input
x.values <- seq(-2, 2, by = 0.1)

# for each x calculate y
n <- length(x.values)

#initiate y-values
y.values <- rep(0, n)

for (i in 1:n) {
  x <- x.values[i]
  if (x <= 0) {
    y <- -x^3
  } else if (x <= 1) {
    y <- x^2
  } else {
    y <- sqrt(x)
  }
  y.values[i] <- y
}

# output
plot(x.values, y.values, type = "l")
############################################OR
for (i in 1:n){
 x<-x.values
 if (x[i]<=0){
	y.values[i]=-x[i]^3}
		else if (x[i]<=1){
		 y.values[i]<-x[i]^2} 
			else{
			 y.values[i]<-sqrt(x[i])
				}
			   }
	
	# output
plot(x.values, y.values, type = "l")


###########################################

# vectorised version
x <- seq(-2, 2, by = 0.1)
y <- ifelse(x <= 0, -x^3, ifelse(x <= 1, x^2, sqrt(x)))
plot(x, y, type = "l", col = "red")