# The following functions have been imported ad verbatim from lattice
# version 0.20-35 and come with the following copyright notice attached.

### Copyright (C) 2001-2006  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
###
### This file is part of the lattice package for R.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA

primName <- function(name, identifier = NULL, name.type = "panel", group = 0) {
  trellis.grobname(name = ifelse(is.null(identifier),
                                 name,
                                 paste(identifier, name, sep = ".")),
                   type = name.type,
                   group = group)
}

hasGroupNumber <- function() {
  aname <- "group.number"
  fnames <- names(formals(sys.function(sys.parent())))
  if (is.na(match(aname, fnames))) {
    if (is.na(match("...", fnames)))
      FALSE
    else {
      dotsCall <- eval(quote(substitute(list(...))), sys.parent())
      !is.na(match(aname, names(dotsCall)))
    }
  } else FALSE
}
