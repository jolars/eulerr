set.seed(1)

n <- 100
p <- 3

fruits <- data.frame(banana = rbinom(),
                     apple  = c(TRUE, FALSE, TRUE, FALSE),
                     orange = c(TRUE, TRUE, TRUE, FALSE))

fruits <- as.data.frame(
  matrix(NA, n, p, dimnames = list(NULL, c("banana", "apple", "orange")))
)

fruits$banana <- rbinom(n, 1, 0.5)

p_apple <- ifelse(fruits$banana, 0.8, 0.2)

fruits$apple <- rbinom(n, 1, p_apple)

p_orange <- ifelse(fruits$banana & fruits$apple, 0.3, 0.1)

fruits$orange <- rbinom(n, 1, p_orange)
fruits <- sapply(fruits, as.logical)
fruits <- as.data.frame(fruits)

fruits$sex <- as.factor(sample(c("male", "female"), n, TRUE))
fruits$age <- as.factor(sample(c("child", "adult"), n, TRUE))

usethis::use_data(fruits, overwrite = TRUE)
