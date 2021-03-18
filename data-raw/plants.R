tmp <- tempfile()

download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/plants/plants.data",
              tmp)

d1 <- readLines(tmp)
d2 <- strsplit(d1, ",", fixed = TRUE)
d3 <- d2[!is.na(d2)]

d4 <- lapply(d3, function(x) x[2:length(x)])
names(d4) <- lapply(d3, "[[", 1)

plants <- d4

usethis::use_data(plants, overwrite = TRUE)

unlink(tmp)
