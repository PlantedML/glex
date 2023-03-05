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
#' @examples
#' # Random Planted Forest -----
#' if (requireNamespace("randomPlantedForest", quietly = TRUE)) {
#' library(randomPlantedForest)
#' rp <- rpf(mpg ~ hp + wt + drat, data = mtcars[1:26, ], max_interaction = 2)
#'
#' glex(rp, mtcars[27:32, ])
#' }
print.glex <- function(x, ...) {

  n <- nrow(x$x)
  n_m <- ncol(x$m)

  cat("glex object of subclass", class(x)[[2]], "\n")
  cat("Explaining predictions of", n, "observations with", n_m, "terms:\n\n")
  #cat("Components $m containing", n_m, "terms :\n")
  str(x, list.len = 5)
}
