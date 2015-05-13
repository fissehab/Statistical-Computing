# estimate probability of four H in a sequence of length 20

fourH <- function(x) {
  # assumes x is a 0-1 vector of length 20
  # returns TRUE if x contains a sequence of four 1's, o/w returns FALSE
  for (i in 1:17) {
    if (prod(x[i:(i+3)])) return(T)
  }
  return(F)
}

n <- 1e5  # sample size
S <- 0  # count for number of sequences with four H
for (i in 1:n) {
  x <- round(runif(20))
  S <- S + fourH(x)
}
print(S/n)


# calculate probability of four H in a sequence of length 20
# by complete enumeration

S <- 0  # count for number of sequences with four H
for (x01 in 0:1) {
for (x02 in 0:1) {
for (x03 in 0:1) {
for (x04 in 0:1) {
for (x05 in 0:1) {
for (x06 in 0:1) {
for (x07 in 0:1) {
for (x08 in 0:1) {
for (x09 in 0:1) {
for (x10 in 0:1) {
for (x11 in 0:1) {
for (x12 in 0:1) {
for (x13 in 0:1) {
for (x14 in 0:1) {
for (x15 in 0:1) {
for (x16 in 0:1) {
for (x17 in 0:1) {
for (x18 in 0:1) {
for (x19 in 0:1) {
for (x20 in 0:1) {
  x <- c(x01,x02,x03,x04,x05,x06,x07,x08,x09,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  S <- S + fourH(x)
}}}}}}}}}}}}}}}}}}}}
print(S/2^20)


# calculate probability of four H in a sequence of length 20
# by complete enumeration, version 2

allbin <- function(n) {
  # generates all binary sequences of length n, as rows of a matrix
  if (n == 1) {
    return(0:1)
  } else {
    x <- allbin(n-1)
    k <- 2^(n-1)
    return( rbind( cbind(rep(0,k), x), cbind(rep(1,k), x) ) )
  }
}

x <- allbin(20)
S <- 0  # count for number of sequences with four H
for (i in 1:(2^20)) {
  S <- S + fourH(x[i,])
}
print(S/2^20)

# or, in one line
print(sum(apply(x, 1, fourH))/2^20)
