#' Group terms into higher order features
#'
#' For the use of one-hot-encoded features, it is typically easier
#' to interpret results if terms belong to each encoded feature are re-
#' aggregated into the original features.
#'
#' @param object (`glex`) Object of class `glex`.
#' @param groups (`list`) Named list of vectors with the encoded term names.
#'   The list named denote the name of the new feature, containing the terms enumerated
#'   in the vector, e.g. `list(x1 = c("x1.A", "x1.B", "x1.C"))` would group
#'   dummy-encoded terms `x1.A`, `x1.B`, and `x1.C` into a feature `x1`.
#'
#' @return Modified obect of class `glex`.
#'
#' @examples
#'
#'
group_features <- function(object, groups = NULL, ...) {
  if (is.null(groups)) {
    return(object)
  }

  if (!is.list(groups)) {
    stop("Argument 'groups' must be a list.")
  }

  if (!all(sapply(groups, is.character))) {
    stop("All elements of 'groups' must be character vectors.")
  }

  # if (!all(sapply(groups, function(x) all(x %in% object$terms))) {
  #   stop("All elements of 'groups' must be subsets of 'object$terms'.")
  # }

  new_terms <- unlist(groups)
  new_terms <- setdiff(new_terms, object$terms)
  new_terms <- c(object$terms, new_terms)

  new_data <- object$data
  for (feature in names(groups)) {
    new_data[[feature]] <- rowSums(object$data[groups[[feature]]])
  }

  new_object <- object
  new_object$terms <- new_terms
  new_object$data <- new_data

  return(new_object)
}
