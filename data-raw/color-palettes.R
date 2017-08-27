# Generate a set of qualitative color palettes using qualpalr -------------

palette <- qualpalr::autopal(15,
                             cvd = "deutan",
                             colorspace = "pretty")$hex

# Make manual adjustments -------------------------------------------------

palette[10] <- palette[4]
palette[4]  <- palette[1]
palette[2]  <- "white"
palette[1]  <- "grey"
palette[3]  <- "steelblue4"
palette[7]  <- "grey30"

# Save as a data file to be used inside eulerr ----------------------------

devtools::use_data(palette,
                   internal = TRUE,
                   compress = "gzip",
                   overwrite = TRUE)
