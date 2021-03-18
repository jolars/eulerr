pain <- structure(c(78L, 143L, 48L, 204L, 229L, 43L),
                  .Dim = 3:2,
                  .Dimnames = structure(
                    list(c("widespread", "regional", "local"),
                         c("male", "female")),
                    .Names = c("pain", "sex")
                  ),
                  class = "table")

usethis::use_data(pain, overwrite = TRUE)
