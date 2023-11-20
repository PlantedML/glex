#' Partial Dependence Plot
#'
#' A version of [`plot_main_effect`] with the intercept term (horizontal line) added,
#' resulting in a partial dependence plot.
#'
#' @param object Object of class [`glex`].
#' @param predictor (`character(1)`) predictor names, e.g. `"x1"`.
#' @param ... Used for future expansion.
#'
#' @return A `ggplot2` object.
#' @import ggplot2
#' @export
#' @seealso [plot_main_effect()]
#' @family Visualization functions
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
#' plot_pdp(components, "wt")
#' plot_pdp(components, "cyl")
#' }
plot_pdp <- function(object, predictor, ...) {
  plot_main_effect_impl(object, predictor, pdp = TRUE)
}
