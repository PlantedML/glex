#' @rdname plot_components
#' @export
autoplot.glex <- function(object, predictors, ...) {
  np <- length(predictors)

  if (np == 1) p <- plot_main_effect(object, predictors, ...)
  if (np == 2) p <- plot_twoway_effects(object, predictors, ...)
  if (np == 3) p <- plot_threeway_effects(object, predictors, ...)

  p
}
