# Optimization function for disc_disc intersection ------------------------

separate_two_discs <- function(r1, r2, overlap) {
  optimize(
    function(x, r1, r2, overlap) (discdisc(r1, r2, d = x) - overlap) ^ 2L,
    interval = c(abs(r1 - r2), sum(r1, r2)),
    r1 = r1,
    r2 = r2,
    overlap = overlap,
    tol = sqrt(.Machine$double.eps)
  )$minimum
}

