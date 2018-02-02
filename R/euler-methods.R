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


#' Fitted values of euler object
#'
#' @param object object of class `'euler'`
#' @param ... ignored
#' @keywords internal
#' @export
#' @return fitted values
fitted.euler <- function(object, ...) {
  object$fitted.values
}

#' Return ellipses from the euler object
#'
#' @param object object of class `'euler'`
#' @param ... ignored
#' @export
#' @keywords internal
#' @return a data frame of the ellipses in the fit
coef.euler <- function(object, ...) {
  object$ellipses
}
