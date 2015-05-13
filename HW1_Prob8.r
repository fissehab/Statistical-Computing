# HW1_Prob8


# different ways of rolling k dice
k <- 4
ceiling(6*runif(k))
ceiling(runif(k, 0, 6))
sample(1:6, size = k, replace = TRUE)

# part (a)

win <- FALSE
for (i in 1:4) {
  if (sample(1:6, size = 1) == 6) {
    win <- TRUE
  }
}
if (win) {
  print("win")
} else {
  print("lose")
}

# part (b)

sixes <- function(n = 4) {
  # plays the game of the Chevalier de Mere
  # returns TRUE if at least one six in n rolls
  # returns FALSE otherwise
  win <- FALSE
  for (i in 1:n) {
    if (sample(1:6, size = 1) == 6) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# vectorised version of above
sixes <- function(n = 4) {
  sum(sample(1:6, size = n, replace = TRUE) == 6) > 0
}

# part (c)

p_estimate <- function(N, n = 4) {
  # proportion of wins in N runs of sixes(n)
  total_wins <- 0
  for (i in 1:N) {
    if (sixes(n)) total_wins <- total_wins + 1
  }
  return(total_wins/N)
}

p_accuracy <- function(N, n = 4) {
  # accuracy of p_estimate
  total_wins <- 0
  for (i in 1:N) {
    if (sixes(n)) total_wins <- total_wins + 1
  }
  return(total_wins/N - 1 + (5/6)^n)
}

# part (d)

sixes_sim <- function(N, n = 4) {
  # runs sixes(n) N times and saves the results in "sixes_sim.txt"
  cat(file="sixes_sim.txt") # deletes contents of file
  for (i in 1:N) {
    cat(file = "sixes_sim.txt", sixes(n), "\n", append = TRUE)
  }
}

sixes_sim(100)
results <- scan("sixes_sim.txt", what = TRUE)
mean(results)
  
