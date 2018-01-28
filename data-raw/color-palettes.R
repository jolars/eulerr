# Generate a set of qualitative color palettes using qualpalr -------------

pal <- qualpalr::autopal(15,
                         cvd = "deutan",
                         colorspace = "pretty")$hex

# Make manual adjustments -------------------------------------------------

palette <- pal
palette[1] <- "white"
palette[2] <- "grey70"
palette[3] <- "steelblue4"
palette[4] <- pal[1]
palette[7] <- "grey30"
palette[8] <- pal[4]
palette[10] <- pal[8]

# Save as a data file to be used inside eulerr ----------------------------

devtools::use_data(palette,
                   internal = TRUE,
                   compress = "gzip",
                   overwrite = TRUE)
