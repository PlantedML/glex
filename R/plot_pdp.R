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
  checkmate::assert_class(object, "glex")
  checkmate::assert_string(predictor) # Must be a single predictor
  checkmate::assert_subset(predictor, colnames(object$x), empty.ok = FALSE)

  xdf <- assemble_components(object, predictor)
  # Add intercept - only relevant difference to plot_main_effect
  xdf[["m"]] <- xdf[["m"]] + object[["intercept"]]
  x_type <- get_x_types(object, predictor)

  if (x_type == "continuous") {
    p <- ggplot2::ggplot(xdf, ggplot2::aes(
      x = .data[[predictor]], y = .data[["m"]])
    )
    p <- p + ggplot2::geom_line(linewidth = 1.2, key_glyph = "rect", color = "#194155")
  }

  if (x_type == "categorical") {
    p <- ggplot2::ggplot(xdf, ggplot2::aes(x = .data[[predictor]], y = .data[["m"]]))
    p <- p + ggplot2::geom_col(alpha = .8, width = 1/2, fill = "#194155")
    p <- p + theme(panel.grid.major.x = element_blank())
  }

  if (!is.null(object$target_levels)) {
    p <- p + facet_wrap(vars(.data[["class"]]), labeller = label_both)
  }

  p +
    ggplot2::geom_hline(yintercept = object[["intercept"]], linetype = "6161") +
    ggplot2::labs(y = "Prediction")
}
