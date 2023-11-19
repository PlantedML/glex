#' Subset components
#'
#' @param components An object of class `glex`.
#' @param term (`character(1)`) A main term name to subset by, e.g. `"x1"`.
#'
#' @return
#' - `subset_components`: An object of class `glex`.
#' - `subset_component_names`: A character vector.
#'
#' @rdname subset_components
#' @export
#' @examples
#' if (requireNamespace("randomPlantedForest", quietly = TRUE)) {
#' library(randomPlantedForest)
#'
#' # introduce factor variables to show categorical feature handling
#' mtcars$cyl <- factor(mtcars$cyl)
#' mtcars$vs <- factor(mtcars$vs)
#'
#' # Fit forest, get components
#' set.seed(12)
#' rpfit <- rpf(mpg ~ cyl + wt + hp + drat + vs, data = mtcars, ntrees = 25, max_interaction = 3)
#' components <- glex(rpfit, mtcars)
#'
#' # Get component object with only "hp" and its interactions
#' subset_components(components, "hp")
#'
#' subset_component_names(components, "hp")
#' }
#'
subset_components <- function(components, term) {
  checkmate::assert_string(term)
  components$m <- components$m[, find_term_matches(main_term = term, names(components$m)), with = FALSE]
  components$x <- components$x[, find_term_matches(main_term = term, names(components$x)), with = FALSE]
  components
}

#' @rdname subset_components
#' @export
subset_component_names <- function(components, term) {
  checkmate::assert_string(term)
  names(components$m)[find_term_matches(main_term = term, names(components$m))]
}

#' Get the degree of interaction from vector of terms
#'
#' Terms of the form `"x1", "x2", "x1:x2"` have degrees 1, 1, 2, respectively.
#' This utility function exists mainly for code deduplication and consistency.
#' It is also a lot faster than using regex-approaches, yet it still makes the strong assumption
#' of `:` _only_ occuring as an interaction delimiter.
#'
#' @noRd
#' @keywords internal
get_degree <- function(x, pattern = ":") {
  lengths(strsplit(x, split = pattern, fixed = TRUE))
}
