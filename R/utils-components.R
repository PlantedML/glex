#' Subset components
#'
#' @param components An object of class `glex`.
#' @param term (`character(1)`) A main term name to subset by, e.g. `"x1"`.
#'
#' @returns
#' - `subset_components()`: An object of class `glex`.
#' - `subset_component_names()`: A character vector.
#'
#' @rdname subset_components
#' @export
#' @examplesIf requireNamespace("randomPlantedForest", quietly = TRUE)
#' library(randomPlantedForest)
#'
#' # introduce factor variables to show categorical feature handling
#' mtcars$cyl <- factor(mtcars$cyl)
#' mtcars$vs <- factor(mtcars$vs)
#'
#' # Fit forest, get components
#' set.seed(12)
#' # rpf() prunes by default from randomPlantedForest 0.6; prune = FALSE keeps the forest
#' new_rpf <- "prune_components" %in% getNamespaceExports("randomPlantedForest")
#' forest <- if (new_rpf) list(prune = FALSE)
#' rpfit <- do.call(rpf, c(
#'   list(mpg ~ cyl + wt + hp + drat + vs, data = mtcars, max_interaction = 3),
#'   forest
#' ))
#' components <- glex(rpfit, mtcars)
#'
#' # Get component object with only "hp" and its interactions
#' subset_components(components, "hp")
#'
#' subset_component_names(components, "hp")
#'
subset_components <- function(components, term) {
  checkmate::assert_string(term)
  components$m <- components$m[,
    find_term_matches(main_term = term, names(components$m)),
    with = FALSE
  ]
  components$x <- components$x[,
    find_term_matches(main_term = term, names(components$x)),
    with = FALSE
  ]
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
#' The `__class:<level>` suffix of multiclass terms is not part of the term.
#' Otherwise `:` is assumed to occur only as the interaction delimiter.
#'
#' @noRd
#' @keywords internal
get_degree <- function(x, pattern = ":") {
  x <- sub("__class:.*$", "", x)
  lengths(strsplit(x, split = pattern, fixed = TRUE))
}
