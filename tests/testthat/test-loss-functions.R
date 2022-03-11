library(eulerr)

s <- list()

s[[1]] <- c("A" = 10, "B" = 10, "C" = 10, "A&B" = 4, "A&C" = 4, "B&C" = 4, "A&B&C" = 2)
s[[3]] <- c("A" = 10, "B" = 10, "C" = 0, "A&B" = 4, "A&C" = 0, "B&C" = 0, "A&B&C" = 3)

s[[5]] <- c("A" = 15, "B" = 15, "C" = 0, "A&B" = 3, "A&C" = 0, "B&C" = 0, "A&B&C" = 3)


s[[9]] <- c(A = 4, B = 6, C = 3, D = 2, E = 7, F = 3, "A&B" = 2, "A&F" = 2, "B&C" = 2, "B&D" = 1, "B&F" = 2, "C&D" = 1, "D&E" = 1, "E&F" = 1, "A&B&F" = 1, "B&C&D" = 1)


s[[10]] <- c("SE" = 13, "Treat" = 28, "Anti-CCP" = 101, "DAS28" = 91, "SE&Treat" = 1, "SE&DAS28" = 14, "Treat&Anti-CCP" = 6, "SE&Anti-CCP&DAS28" = 1)


s[[14]] <- c("A" = 10, "B" = 10, "C" = 10, "D" = 10, "A&B" = 3, "A&C" = 3, "A&D" = 0, "B&C" = 0, "B&D" = 3, "C&D" = 3, "A&B&C" = 1, "A&B&D" = 1, "A&C&D" = 1, "B&C&D" = 1, "A&B&C&D" = 1)

r1 <- euler(s[[14]], loss = "sse")
plot(r1)
r2 <- euler(s[[14]], loss = "sse_max")
plot(r2)

list_euler <- c(
  "agc" = 9, "camk" = 17, "cmgc" = 16, "tk" = 16, "tkl" = 23,
  "agc&camk" = 1,
  "camk&tk" = 1,
  "tk&tkl" = 1,
  "camk&cmgc&tkl" = 1,
  "camk&tk&tkl" = 2,
  "agc&camk&tk&tkl" = 1,
  "camk&cmgc&tk&tkl" = 3,
  "agc&camk&cmgc&tk&tkl" = 1
)

fit <- euler(list_euler, shape = "ellipse", loss = "diag_error")
