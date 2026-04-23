library(eulerr)

data(fruits, package = "eulerr")
fit <- euler(fruits[, 1:5], by = list(age, sex))

# 1) Unnamed labels: interpreted in display order
plot(
  fit,
  strips = list(
    labels = list(
      top = c("Women", "Men"),
      left = c("Children", "Adults")
    )
  )
)

# 2) Named labels: matched by factor levels, then reordered to display
order
plot(
  fit,
  strips = list(
    labels = list(
      top = c(male = "M", female = "F"),
      left = c(adult = "18+", child = "<18")
    )
  )
)

# 3) Expressions also work
plot(
  fit,
  strips = list(
    labels = list(
      top = str2expression(c("alpha[1]", "beta[2]")),
      left = str2expression(c("gamma", "delta"))
    )
  )
)

plot(fit, strips = list(labels = list(top = "Only one")))
