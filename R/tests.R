# # # Test sets ---------------------------------------------------------------
# #
# venn_string <- c(A = 80, B = 150, C = 50, D = 70,
#                  "A&B" = 80, "A&C" = 20, "A&D" = 20, "B&C" = 30, "B&D" = 0, "C&D" = 0,
#                  "A&B&C" = 15, "A&B&D" = 0, "A&C&D" = 0, "B&C&D" = 0,
#                  "A&B&C&D" = 0)
#
# venn_string <- c(A = 20, B = 16, C = 12,
#                  "B&A" = 16, "A&C" = 4, "B&C" = 0,
#                  "A&B&C" = 0)
#
venn_string <- c(SE = 13, Treat = 28, AntiCCP = 101, DAS28 = 91, "SE&Treat" = 1,
                 "SE&DAS28" = 14, "Treat&AntiCCP" = 6, "SE&AntiCCP&DAS28" = 1)

# venn_string <- c(A = 3, B = 2, C = 1.5, A.B = 1, A.C = 1, B.C = 0.75, A.B.C = 0.5)
#
venn_string <- c("B&C" = 2, B = 6, A = 4, C = 3, D = 2, E = 7, F = 3,
                 "A&B" = 2, "F&A" = 2, "B&D" = 1, "D&F" = 0, "B&E" = 0,
                 "B&F" = 2, "C&D" = 1, "D&E" = 1, "E&F" = 1,
                 "A&B&F" = 1, "C&B&D" = 1)

x <- runif(4)
y <- runif(4)
r <- runif(4, .1, .5)

# venn_string <- c(A = 50, B = 120, B.A = 0)
# venn_string <- c(A = 5, B = 6, C = 3, "A;B" = 5, A.C = 2, C.B = 1)
#
