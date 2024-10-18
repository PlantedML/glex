#' @rdname plot_components
#' @export
#' @examples
#' # plot_threeway_effects(components, c("hr", "temp", "workingday"))
plot_threeway_effects <- function(object, predictors, rug_sides = "b", ...) {
  checkmate::assert_class(object, "glex")
  checkmate::assert_character(predictors, len = 3, unique = TRUE)
  checkmate::assert_subset(predictors, names(object$x), empty.ok = FALSE)

  # Create look-up table for predictors and their types
  x_types <- get_x_types(object, predictors)
  xdf <- assemble_components(object, predictors)
  x_cat <- names(xdf)[which(x_types == "categorical")]
  x_cont <- names(xdf)[which(x_types == "continuous")]

  # 3x continuous ----
  if (all(x_types == "continuous")) {
    stop("Can't visualize 3 continuous predictor effects (yet?), feel free to make a suggestion!")
  }

  # 1x categorical 2x continuous ----
  if (all(sort(x_types) == c("categorical", "continuous", "continuous"))) {

    p <- ggplot2::ggplot(xdf, ggplot2::aes(
      x = .data[[x_cont[[1]]]],
      y = .data[[x_cont[[2]]]],
      colour = .data[["m"]],
      fill = ggplot2::after_scale(.data[["colour"]])
    ))
    p <- p + ggplot2::geom_point(size = 2.5, shape = 21, stroke = 1)
    if (rug_sides != "none") {
      p <- p + ggplot2::geom_rug(sides = rug_sides, color = "#1e1e1e")
    }
    p <- p + diverging_palette(limits = get_m_limits(xdf))
    p <- p + ggplot2::labs(color = label_m(predictors))

    if (!is.null(object$target_levels)) {
      p <- p + facet_grid(cols = vars(.data[["class"]]), rows = vars(.data[[x_cat]]), labeller = label_both)
    } else {
      p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[x_cat]]))
    }
  }

  # 2x categorical 1x continuous ----
  if (all(sort(x_types) == c("categorical", "categorical", "continuous"))) {

    p <- ggplot2::ggplot(xdf, ggplot2::aes(
      x = .data[[x_cont]],
      y = .data[["m"]],
      colour = .data[[x_cat[[1]]]],
      fill = ggplot2::after_scale(.data[["colour"]])
    ))
    p <- p + ggplot2::geom_line(linewidth = 1.2, key_glyph = "rect")
    if (rug_sides != "none") {
      p <- p + ggplot2::geom_rug(sides = rug_sides, color = "#1e1e1e")
    }
    p <- p + ggplot2::scale_color_brewer(palette = "Dark2")
    p <- p + ggplot2::labs(y = label_m(predictors))

    if (!is.null(object$target_levels)) {
      p <- p + facet_grid(cols = vars(.data[["class"]]), rows = vars(.data[[x_cat[[2]]]]), labeller = label_both, scales = "free_y")
    } else {
      p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[x_cat[[2]]]]), scales = "free_y")
    }
  }

  # 3x categorical ----
  if (all(x_types == "categorical")) {

    p <- ggplot2::ggplot(xdf, ggplot2::aes(
      x = .data[[x_cat[[1]]]],
      y = .data[[x_cat[[2]]]],
      colour = .data[["m"]],
      fill = ggplot2::after_scale(.data[["colour"]])
    )) +
      ggplot2::geom_tile() +
      diverging_palette(limits = get_m_limits(xdf)) +
      ggplot2::labs(
        color = label_m(predictors)
      )

    if (!is.null(object$target_levels)) {
      p <- p + facet_grid(cols = vars(.data[["class"]]), rows = vars(.data[[x_cat[[3]]]]), labeller = label_both)
    } else {
      p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[x_cat[[3]]]]))
    }
  }

  # Final cleanup ----
  p + theme_glex()
}
