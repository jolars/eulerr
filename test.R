library(eulerr)

fit <- euler(c(A = 10, B = 8, "A&B" = 3), input = "disjoint")

plot(fit, bg = "red")
