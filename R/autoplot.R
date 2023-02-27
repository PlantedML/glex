#' @rdname plot_components
#' @export
autoplot.glex <- function(object, predictors, ...) {
  np <- length(predictors)

  if (np == 1) p <- plot_main_effect(object, predictors, ...)
  if (np == 2) p <- plot_twoway_effects(object, predictors, ...)
  if (np == 3) p <- plot_threeway_effects(object, predictors, ...)

  p
}

#' Plot glex Variable Importances
#'
#' @param object Object of class `glex_vi`, see [`glex_vi()`].
#' @param by_degree (`FALSE`) Optionally sum values by degree of interaction, resulting in one contribution score
#'  for all main effects, all second-order interactions, etc.
#' @param scale (`"absolute"`) Plot average absolute contributions (default) or the same value but scaled by the
#' average prediction (`"relative"`).
#' @param ... (Unused)
#' @inheritParams glex_vi
#'
#' @return A `ggplot2` object.
#' @export
#' @seealso glex_vi
autoplot.glex_vi <- function(object, threshold = 0, by_degree = FALSE, scale = "absolute", ...) {

  checkmate::assert_flag(by_degree)
  checkmate::assert_number(threshold, lower = 0, finite = TRUE)
  checkmate::assert_subset(scale, choices = c("absolute", "relative"))

  object <- data.table::copy(object)
  by_what <- ifelse(by_degree, "Degree", "Term")
  score <- switch(scale, absolute = "m", relative = "m_rel")

  # FIXME: data.table NSE stuff
  m <- m_rel <- NULL

  object <- object[m_rel >= threshold]

  if (by_what == "Degree") {

    aggr <- object[, list(m = sum(m), m_rel = sum(m_rel)), by = "degree"]

    p <- ggplot(aggr)
    p <- p + aes(y = stats::reorder(.data[["degree"]], .data[[score]]))

    x_lab <- switch(
      score,
      m = "Sum of Average Absolute Contributions",
      m_rel = "Sum of Average Absolute Contributions\nRelative to Average Prediction"
    )
  }

  if (by_what == "Term") {

    p <- ggplot(object)
    p <- p + aes(y = stats::reorder(.data[["term"]], .data[[score]]))

    x_lab <- switch(
      score,
      m = "Average Absolute Contribution",
      m_rel = "Average Absolute Contribution\nRelative to Average Prediction"
    )
  }

  p + aes(x = .data[[score]], fill = factor(.data[["degree"]])) +
    geom_col(linewidth = 0, alpha = .8) +
    coord_cartesian(xlim = c(0, NA), expand = FALSE) +
    scale_fill_viridis_d(direction = -1, end = .95) +
    labs(
      title = sprintf("glex Variable Importances by %s", by_what),
      y = by_what, x = x_lab,
      fill = "Degree of Interaction"
    ) +
    theme(
      legend.position = "bottom",
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}
