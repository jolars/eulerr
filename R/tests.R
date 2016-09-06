# Libraries ---------------------------------------------------------------

library(plotrix)
library(RColorBrewer)
library(optimx)


# Test sets ---------------------------------------------------------------

venn_string <- c(A = 80, B = 150, C = 50, D = 70,
                 A.B = 80, A.C = 20, A.D = 20, B.C = 30, B.D = 0, C.D = 0,
                 A.B.C = 15, A.B.D = 0, A.C.D = 0, B.C.D = 0,
                 A.B.C.D = 0)

venn_string <- c(A = 20, B = 16, C = 12,
                 B.A = 16, A.C = 4, B.C = 0,
                 A.B.C = 0)

venn_string <- c(SE = 13, Treat = 28, AntiCCP = 101, DAS28 = 91, SE.Treat = 1,
                 SE.DAS28 = 14, Treat.AntiCCP = 6, SE.AntiCCP.DAS28 = 1)

venn_string <- c(A = 3, B = 2, C = 1.5, A.B = 1, A.C = 1, B.C = 0.75, A.B.C = 0.5)

venn_string <- c(B.C = 2, B = 6, A = 4, C = 3, D = 2, E = 7, F = 3,
                 A.B = 2, F.A = 2, B.D = 1, D.F = 0, B.E = 0,
                 B.F = 2, C.D = 1, D.E = 1, E.F = 1,
                 A.B.F = 1, C.B.D = 1)

venn_string <- c(A = 50, B = 120, B.A = 0)
venn_string <- c(A = 5, B = 6, C = 3, "A;B" = 5, A.C = 2, C.B = 1)

initial_layout <- optimx(
  par = runif(length(oneset_areas) * 2, 0, min(radiuses)),
  fn = initial_layout_optimizer,
  gr = initial_layout_gradient,
  distances = twoset_distances,
  disjoint = disjoint,
  contained = contained,
  #lower = rep(0, times = length(oneset_areas) * 2),
  #upper = rep(sum(radiuses) * 2, times = length(oneset_areas) * 2),
  method = c("L-BFGS-B", "nlminb", "newuoa", "bobyqa")
)

circles <- matrix(c(coef(initial_layout)[1, ], radiuses), dimnames = list(setnames, c("x", "y", "r")),
                  ncol = 3)

# Calculate the areas in the current solution
# Begin calculating two-set interactions, which is easy.


init <- as.vector(circles[, 1:2])

fit2 <- optimx(init, compute_area_diff, r = radiuses,
               #lower = rep(0, times = length(oneset_areas) * 2),
               #upper = rep(sum(radiuses) * 2, times = length(oneset_areas) * 2),
               method = c("newuoa", "bobyqa", "nlminb", "nlm"))
               #control = list(all.methods = T))

fit3 <- optim(init, compute_area_diff, r = radiuses, method = "L-BFGS-B",
              lower = rep(0, times = length(oneset_areas) * 2),
              upper = rep(sum(radiuses) * 2, times = length(oneset_areas) * 2))

circles <- matrix(c(coef(fit2)[2, ], radiuses), dimnames = list(setnames, c("x", "y", "r")),
                  ncol = 3)
circles <- matrix(c(fit3$par, radiuses), dimnames = list(setnames, c("x", "y", "r")),
                  ncol = 3)
circles <- matrix(c(init, radiuses), dimnames = list(setnames, c("x", "y", "r")),
                  ncol = 3)

initial_par <- coef(initial_layout)[1, ]


final_layout <- final_layout_optimizer(
  par = initial_par,
  r = radiuses,
  all_areas = all_areas,
  oneset_areas = oneset_areas,
  twoset_areas = twoset_areas,
  twoset_names = twoset_names
)




