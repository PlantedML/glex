#' @rdname plot_components
#' @param resolution (`integer(1): 100`) Models of class `rpf_pruned` only: the most cells of a heat map along each
#'   predictor, see Details.
#' @export
#' @examplesIf requireNamespace("randomPlantedForest", quietly = TRUE)
#' library(randomPlantedForest)
#'
#' # 2-degree interaction effects ----
#' # 2d continuous, scatterplot of arbitrary orientation
#' plot_twoway_effects(components, c("wt", "drat"))
#' # flipped: plot_twoway_effects(components, c("drat", "wt"))
#'
#' # continuous + categorical (forces continuous on x axis, colors by categorical)
#' plot_twoway_effects(components, c("wt", "cyl"))
#' # identical: plot_twoway_effects(components, c("cyl", "wt"))
#'
#' # 2d categorical, heatmap of arbitrary orientation
#' plot_twoway_effects(components, c("vs", "cyl"))
#' plot_twoway_effects(components, c("cyl", "vs"))
plot_twoway_effects <- function(
  object,
  predictors,
  rug_sides = "b",
  resolution = 100L,
  ...
) {
  checkmate::assert_class(object, "glex")
  checkmate::assert_character(predictors, len = 2, unique = TRUE)
  checkmate::assert_subset(predictors, names(object$x))
  if (is_pruned_glex(object)) {
    checkmate::assert_int(resolution, lower = 2)
    return(plot_pruned_pair(object, predictors, resolution = resolution))
  }

  x_types <- get_x_types(object, predictors)
  xdf <- assemble_components(object, predictors)
  x_cat <- names(xdf)[which(x_types == "categorical")]
  x_cont <- names(xdf)[which(x_types == "continuous")]

  fillaes <- ggplot2::aes(
    color = .data[["m"]],
    fill = ggplot2::after_scale(.data[["colour"]])
  )

  # 2x continuous ----
  if (setequal(x_types, "continuous")) {
    p <- ggplot2::ggplot(
      xdf,
      ggplot2::aes(
        x = .data[[x_cont[[1]]]],
        y = .data[[x_cont[[2]]]],
        !!!fillaes
      )
    )
    p <- p + ggplot2::geom_point(size = 2.5, shape = 21, stroke = 1)
    p <- p + diverging_palette(limits = get_m_limits(xdf))
    p <- p + ggplot2::labs(color = label_m(predictors))
    if (rug_sides != "none") {
      p <- p + ggplot2::geom_rug(sides = rug_sides, color = "#1e1e1e")
    }
  }

  # 2x categorical ----
  if (setequal(x_types, "categorical")) {
    p <- ggplot2::ggplot(
      xdf,
      ggplot2::aes(
        x = .data[[x_cat[[1]]]],
        y = .data[[x_cat[[2]]]],
        !!!fillaes
      )
    )
    p <- p + ggplot2::geom_tile()
    p <- p + diverging_palette(limits = get_m_limits(xdf))
    p <- p + ggplot2::labs(color = label_m(predictors))
  }

  # 1x categorical 1x continuous ----
  if (setequal(x_types, c("categorical", "continuous"))) {
    p <- ggplot2::ggplot(
      xdf,
      ggplot2::aes(
        x = .data[[x_cont]],
        y = .data[["m"]],
        color = .data[[x_cat]]
      )
    )
    p <- p + ggplot2::geom_line(linewidth = 1.2, key_glyph = "rect")
    p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "6161")
    if (rug_sides != "none") {
      p <- p + ggplot2::geom_rug(sides = rug_sides, color = "#1e1e1e")
    }
    p <- p + discrete_palette()
    p <- p + ggplot2::labs(y = label_m(predictors))
  }

  if (!is.null(object$target_levels)) {
    p <- p + facet_wrap(vars(.data[["class"]]), labeller = label_both)
  }
  # Final cleanup ----
  p + theme_glex()
}
