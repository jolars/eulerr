# Exclusive region sizes from the medium and hard cases in eunoia-py's corpus.
corpus_source <- paste0(
  "https://github.com/jolars/eunoia-py/blob/",
  "8eae9277dd48484f441c9fdf4b273a5cd7291de1/benchmarks/cases.py"
)

datasets <- list(
  uniform_3_set = c(
    A = 10,
    B = 10,
    C = 10,
    "A&B" = 4,
    "A&C" = 4,
    "B&C" = 4,
    "A&B&C" = 2
  ),
  eulerape_3_set = c(
    a = 3491,
    b = 3409,
    c = 3503,
    "a&b" = 120,
    "a&c" = 114,
    "b&c" = 132,
    "a&b&c" = 126
  ),
  issue47_3_set_huge_triple = c(
    A = 500,
    B = 400,
    C = 400,
    "A&B" = 30,
    "A&C" = 40,
    "B&C" = 15,
    "A&B&C" = 120
  ),
  issue111_3_set_asymmetric = c(
    A = 10000,
    B = 1000,
    C = 100,
    "A&B" = 50,
    "A&C" = 30,
    "B&C" = 260,
    "A&B&C" = 15
  ),
  issue114_4_set_dominant_quad = c(
    A = 7516,
    B = 7621,
    C = 3152,
    D = 26642,
    "A&B" = 781,
    "A&C" = 817,
    "A&D" = 6418,
    "B&C" = 369,
    "B&D" = 1465,
    "C&D" = 4118,
    "A&B&C" = 324,
    "A&B&D" = 2525,
    "A&C&D" = 8847,
    "B&C&D" = 1149,
    "A&B&C&D" = 10336
  ),
  issue103_4_set = c(
    A = 26,
    B = 455,
    C = 86,
    D = 26,
    "A&B" = 10,
    "A&C" = 6,
    "A&D" = 4,
    "B&C" = 34,
    "B&D" = 56,
    "C&D" = 21,
    "A&B&C" = 2,
    "A&B&D" = 8,
    "A&C&D" = 13,
    "B&C&D" = 79,
    "A&B&C&D" = 51
  ),
  issue93_5_set_kinases = c(
    agc = 9,
    camk = 17,
    cmgc = 16,
    tk = 16,
    tkl = 23,
    "agc&camk" = 1,
    "camk&tk" = 1,
    "tk&tkl" = 1,
    "camk&cmgc&tkl" = 1,
    "camk&tk&tkl" = 2,
    "agc&camk&tk&tkl" = 1,
    "camk&cmgc&tk&tkl" = 3,
    "agc&camk&cmgc&tk&tkl" = 1
  ),
  wilkinson_6_set = c(
    A = 4,
    B = 6,
    C = 3,
    D = 2,
    E = 7,
    F = 3,
    "A&B" = 2,
    "A&F" = 2,
    "B&C" = 2,
    "B&D" = 1,
    "B&F" = 2,
    "C&D" = 1,
    "D&E" = 1,
    "E&F" = 1,
    "A&B&F" = 1,
    "B&C&D" = 1
  )
)

case_info <- data.frame(
  dataset = names(datasets),
  label = c(
    "Uniform (3 sets)",
    "eulerAPE (3 sets)",
    "Large triple (3 sets)",
    "Asymmetric (3 sets)",
    "Large 4-way (4 sets)",
    "Dense (4 sets)",
    "Kinases (5 sets)",
    "Wilkinson (6 sets)"
  ),
  n_sets = c(3L, 3L, 3L, 3L, 4L, 4L, 5L, 6L),
  stringsAsFactors = FALSE
)
