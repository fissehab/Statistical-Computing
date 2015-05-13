# HW1_prob 2-5
x <- 0.8
n <- 10

# using a for loop
h <- 0
for (i in 0:n) h <- h + x^i
print(h)

#function
arithmetic_sum <- function(x, n) {
  # sum of x^k for k = 0, ..., n
  if (x == 1) {
    return(n + 1)
  } else {
    return((x^(n+1) - 1)/(x - 1))
  }
}

# using the formula
if (x == 1) {
  h <- n + 1
} else {
  h <- (1 - x^(n+1))/(1 - x)
}
print(h)

# using a while loop
h <- 0
i <- 0
while (i <= n) {
  h <- h + x^i
  i <- i + 1
}
print(h)

# vectorised
(h <- sum(x^(0:n)))
