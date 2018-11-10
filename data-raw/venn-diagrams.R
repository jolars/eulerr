# Ellipse specifications for Venn diagrams --------------------------------

library(eulerr)

ell <- function(n) {
  as.data.frame(matrix(
    NA,
    ncol = 5L,
    nrow = n,
    dimnames = list(LETTERS[seq_len(n)], c("h", "k", "a", "b", "phi"))
  ))
}

# One set -----------------------------------------------------------------

s1 <- ell(1)
s1[1, ] <- c(0, 0, 1, 1, 0)

# Two sets ----------------------------------------------------------------

s2 <- ell(2)
s2[, 1] <- c(-0.5, 0.5)
s2[, 2] <- 0
s2[, 3:4] <- 1
s2[, 5] <- 1

# f2 <- euler(c(A = 1, B = 1))
# f2$ellipses <- s2
# plot(f2)

# Three sets --------------------------------------------------------------

f3 <- euler(c(A = 1, B = 1, C = 1,
              "A&B" = 1, "A&C" = 1, "B&C" = 1,
              "A&B&C" = 1))

s3 <- f3$ellipses <- round(f3$ellipses, 2)

# plot(f3)

# Four sets ---------------------------------------------------------------

s4 <- ell(4)
s4[, 1] <- c(-0.8, 0.8, 0, 0)
s4[, 2] <- c(0, 0, 1, 1)
s4[, 3] <- 1.2
s4[, 4] <- 2
s4[, 5] <- round(c(pi/4, -pi/4), 3)

# f4 <- euler(c(A = 1, B = 1, C = 1, D = 1))
# f4$ellipses <- s4
# plot(f4)

# Five sets ---------------------------------------------------------------

t <- seq(0, 2*pi, length.out = 6)[-6]
r <- 0.2

s5 <- ell(5)

s5[, 1] <- r*cos(t + 0.5)
s5[, 2] <- r*sin(t + 0.5)
s5[, 3] <- 1
s5[, 4] <- 0.6
s5[, 5] <- t
s5 <- round(s5, 3)

# f5 <- euler(c(A = 1, B = 1, C = 1, D = 1, E = 1))
# f5$ellipses <- s5
# plot(f5)

# Save specifications -----------------------------------------------------

venn_spec <- list(one = s1,
                  two = s2,
                  three = s3,
                  four = s4,
                  five = s5)

usethis::use_data(venn_spec, internal = TRUE, overwrite = TRUE)
