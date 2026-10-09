#' Print glex objects
#'
#' This is implemented mainly to avoid flooding the console in cases where the `glex` object
#' uses many terms, which leads to a large amount of column names of `$m` being printed to the console.
#' This function wraps [`str()`][utils::str] with a truncated output for a more compact representation.
#' @param x Object to print.
#' @param ... (Unused)
#'
#' @export
#' @importFrom utils str
#' @examplesIf requireNamespace("randomPlantedForest", quietly = TRUE)
#' # Random Planted Forest -----
#' library(randomPlantedForest)
#' rp <- rpf(mpg ~ hp + wt + drat, data = mtcars[1:26, ], max_interaction = 2)
#'
#' glex(rp, mtcars[27:32, ])
print.glex <- function(x, ...) {
  n <- nrow(x$x)
  n_m <- ncol(x$m)
  max_deg <- max(get_degree(names(x$m)))

  cat("glex object of subclass", class(x)[[2]], "\n")
  cat(as.character(cli::pluralize(
    "Explaining predictions of {n} observation{?s} with {n_m} term{?s} of up to {max_deg} degree{?s}"
  )))
  cat("\n")

  if (length(x$constrained) > 0) {
    cat(
      "Decomposition constrained by",
      paste0("`", x$constrained, "`", collapse = " and "),
      "- SHAP values are NA\n"
    )
  }

  cat("\n")
  str(x, list.len = 5)
}
