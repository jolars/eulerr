#' Plants
#'
#' Data on plants and the states in the US and Canada they occur in.
#'
#' @format A [list] with 33,721 plants, each containing a character vector
#'   listing the states in the US and Canada in which they occur. The names in
#'   the list specify the species or genus of the plant.
#'
#' @source USDA, NRCS. 2008. The PLANTS Database (<http://plants.usda.gov/>, 31
#'   December 2008). National Plant Data Center, Baton Rouge, LA 70874-4490 USA.
#' @source Dua, D. and Karra Taniskidou, E. (2017). UCI Machine Learning
#' Repository <http://archive.ics.uci.edu/ml>. Irvine, CA: University of
#' California, School of Information and Computer Science.
"plants"

#' Fruits
#'
#' A synthethic data set of preferences for fruits and their overlaps,
#' generated only to be a showcase for the examples for this package.
#'
#' @format A [data.frame] with 100 observations of 5 variables:
#' \describe{
#'   \item{banana}{whether the person likes bananas, a logical}
#'   \item{apple}{whether the person likes apples, a logical}
#'   \item{orange}{whether the person likes oranges, a logical}
#'   \item{sex}{the sex of the person, a factor with levels 'male' and 'female'}
#'   \item{age}{the age of the person, a factor with levels 'child' and 'adult'}
#' }
"fruits"

#' Organisms
#'
#' Example data from the **VennMaster** package.
#'
#' Note that this data is difficult to fit using an Euler diagram, even
#' if we use ellipses, which is clear if one chooses to study the various
#' overlaps in the resulting diagrams.
#'
#' @format A [matrix] with 7 observations, consisting of various organisms,
#'   and 5 variables: *animal, mammal, plant, sea*, and, *spiny*,
#'   indicating whether the organism belongs to the category or not.
#'
#' @source <https://github.com/sysbio-bioinf/VennMaster/blob/master/data_examples/deploy/example1.list>
"organisms"
