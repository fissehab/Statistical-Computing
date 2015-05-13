# HW1_Prob6
x <- c(3, 4, 2, 6, 1, 4)

x.min <- x[1]
cat('x.min set to', x.min, '\n')
for (xi in x[-1]) {
  if (xi < x.min) {
    x.min <- xi
    cat('x.min changed to', x.min, '\n')
  }
}
print(x.min)