# eulerr: Area-Proportional Euler and Venn Diagrams with Circles or Ellipses
# Copyright (C) 2018 Johan Larsson <johanlarsson@outlook.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Print Euler fits
#'
#' Prints a data frame of the original set relationships and the fitted
#' values as well as `diagError` and `stress` statistics.
#'
#' @param x Euler diagram specification from [euler()].
#' @param round Number of decimal places to round to.
#' @param ... Arguments passed to [base::print.data.frame()].
#'
#' @return Prints the results of the fit.
#'
#' @export
print.euler <- function(x, round = 3, ...) {
  stopifnot(is.numeric(round), length(round) == 1L, round > 0)
  out <- data.frame("original" = x$original.values,
                    "fitted" = x$fitted.values,
                    "residuals" = x$residuals,
                    "regionError" = x$regionError)
  print(round(out, digits = round), ...)
  cat("\n")
  cat("diagError:", round(x$diagError, digits = round), "\n")
  cat("stress:   ", round(x$stress, digits = round), "\n")
}
