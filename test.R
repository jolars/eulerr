# Interactive euler_widget() demo.
# Run with the package loaded from source:
#   devtools::load_all(".")
#   source("test.R")
# Each `euler_widget()` call returns an htmlwidget; printing it opens the
# diagram in the RStudio Viewer (or your default browser via `browse_widget()`).

devtools::load_all(".")

# Helper: force a widget open in a browser (handy outside RStudio, e.g. nvim).
browse_widget <- function(w) {
  f <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(w, f, selfcontained = TRUE)
  utils::browseURL(f)
  invisible(f)
}

# 1. Basic two-set Euler diagram -------------------------------------------
# NOTE: eulerr's default palette starts with white/grey85 (to match
# plot.euler()), which looks pale on a white page. Pass `fills` for vivid
# colors.
fit <- euler(c(A = 10, B = 5, "A&B" = 3))
w1 <- euler_widget(fit, fills = c("#3b82f6", "#ef4444"))
browse_widget(w1) # hover the regions: tooltip shows "A&B: 3", etc.

# 2. Three sets with quantities and percentages ----------------------------
fit3 <- euler(c(
  A = 13, B = 8, C = 4,
  "A&B" = 3, "A&C" = 2, "B&C" = 1,
  "A&B&C" = 1
))
w2 <- euler_widget(fit3, quantities = TRUE)
# print(w2)              # opens in the RStudio Viewer
browse_widget(w2)

# 3. Venn diagram (quantities on by default) -------------------------------
v <- venn(list(
  Group1 = 1:20,
  Group2 = 15:35,
  Group3 = 10:12
))
browse_widget(euler_widget(v))

# 4. Custom fills + a complement (universe) box ----------------------------
fitc <- euler(
  c(Cats = 10, Dogs = 7, "Cats&Dogs" = 3),
  complement = 15
)
w4 <- euler_widget(
  fitc,
  fills = c("#f8766d", "#00bfc4"),
  quantities = TRUE
)
browse_widget(w4) # dashed box = complement; label shows the outside count

# 5. Edges only, no fills (regions still hoverable) ------------------------
browse_widget(euler_widget(fit3, fills = FALSE, quantities = TRUE))
