tmp <- tempfile()
download.file("https://raw.githubusercontent.com/sysbio-bioinf/VennMaster/master/data_examples/deploy/example1.list",
              tmp)

d1 <- read.delim(tmp, header = FALSE)
d2 <- unstack(d1, V1 ~ V2)

row_names <- unique(unlist(d2))
col_names <- names(d2)

m1 <- matrix(FALSE,
             length(row_names),
             length(col_names),
             dimnames = list(row_names, col_names))

for (i in seq_along(d2)) {
  m1[, i] <- row_names %in% d2[[i]]
}

organisms <- m1

usethis::use_data(organisms, overwrite = TRUE)
