
 # Statistical computing
 # Spring 2015
 # Homework-2

#*****************
# 1a
#*****************

four.n.twenty<-function(){
  coins<- round(runif(20))
  
  four_heads_row <-0
  
  for (i in 1:17){
    if (prod(coins[i],coins[i+1],coins[i+2], coins[i+3])==1){
      four_heads_row <-four_heads_row+1}}
  if(four_heads_row>0){four_heads_row<-1}  # if four heads are found in a row 1 is passed else 0 is passed
  four_heads_row
}

#**************
#   1b
#****************

four.n.twenty.prob<-function(N){
  
  F<-replicate(N,four.n.twenty())
  
  y<-sum(F)/N    # probability that we get a sequence of fours heads in a row at some point
  cat("The probability of getting four heads in a row when a coin is tossed 20 times using simulation is ", y, "\n")
}

# four.n.twenty.prob(1000000) gives

   # The probability of getting four heads in a row when a coin is tossed 20 times using simulation is  0.47768


# Using four.n.twenty.prob(N), to be confident that my answer is accurate to two decimal paces, I have to take large Values for N

###
# 2
###

# install.packages('gtools')
# load library

library(gtools)

x <- c(0,1)

# Get all permutations in 20 tosses of a coin

a<-permutations(n=2,r=20,v=x,repeats.allowed=T)

a<-t(a)
a<-as.vector(a)

# number of four heads in a row

four_heads_row <-0

for (i in seq(1,(length(a)-3),4)){
  if (prod(a[i],a[i+1],a[i+2], a[i+3])==1){
    four_heads_row <-four_heads_row+1}}

four_heads_row.prob<-four_heads_row/2^20

cat("The probability of getting four heads in a row when a coin is tossed 20 times is ", four_heads_row.prob, "\n")

# Gives:
# The probability of getting four heads in a row when a coin is tossed 20 times is  0.3125



