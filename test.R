library(eulerr)

fit <- euler(
  c(
    "LUTS" = 98,
    "Symptoms of UTI" = 46,
    "Positive urine culture" = 39,
    "Symptoms of UTI&LUTS" = 33,
    "LUTS&Positive urine culture" = 24,
    "Symptoms of UTI&Positive urine culture" = 22,
    "Symptoms of UTI&LUTS&Positive urine culture" = 18
  ),
  input = "union"
)

n_total <- 1615
cnt <- fit$original.values
lbl <- setNames(sprintf("%s (%.2f%%)", cnt, 100 * cnt / n_total), names(cnt))

plot(fit, quantities = list(labels = lbl), legend = FALSE)

plot(
  fit,
  quantities = list(
    type = c("counts", "percent"),
    total = 1615,
    percent = list(fun = round, digits = 2)
  )
)
