# HW1_Prob7

t <- seq(0, 10, by = 0.01)
R <- sqrt(t)
tha <- 2*pi*t
x <- R*cos(tha)
y <- R*sin(tha)
plot(x, y, type = "l", col = "green")
lines(-x, -y, type = "l", col = "red")
