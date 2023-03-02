#' Plot Prediction Components
#'
#' @rdname plot_components
#' @param object Predicted components including the original data the model was fit on, as
#'   returned by `glex()`
#' @param predictor,predictors `[character]` vector of predictor names, e.g. `"x1"` to plot main effect of `x1`, and
#'   `c("x1", "x2")` to plot the interaction term `x1:x2`.
#' @param ... Unused
#'
#' @return A `ggplot2` object.
#' @import ggplot2
#' @export
#'
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
#' # Main effects ----
#' plot_main_effect(components, "wt")
#' plot_main_effect(components, "cyl")
#' }
plot_main_effect <- function(object, predictor, ...) {
  checkmate::assert_class(object, "glex")
  checkmate::assert_string(predictor) # Must be a single predictor
  checkmate::assert_subset(predictor, colnames(object$x), empty.ok = FALSE)

  xdf <- assemble_components(object, predictor)
  x_type <- get_x_types(object, predictor)

  if (x_type == "continuous") {
    p <- ggplot2::ggplot(xdf, ggplot2::aes(
      x = .data[[predictor]], y = .data[["m"]])
    )
    p <- p + ggplot2::geom_line(linewidth = 1.2, key_glyph = "rect")

    # p <- p + ggplot2::geom_point()
  }

  if (x_type == "categorical") {
    p <- ggplot2::ggplot(unique(xdf), ggplot2::aes(x = .data[[predictor]], y = .data[["m"]]))
    p <- p + ggplot2::geom_col(alpha = .8, width = 2/3)
    p <- p + theme(panel.grid.major.x = element_blank())
  }

  if (!is.null(object$target_levels)) {
    p <- p + facet_wrap(vars(.data[["class"]]), labeller = label_both)
  }

  p +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(y = label_m(predictor))
}
