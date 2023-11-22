#' @rdname plot_components
#' @export
#' @family Visualization functions
autoplot.glex <- function(object, predictors, ...) {
  np <- length(predictors)

  if (np < 1 | np > 3) {
    stop("You need to specify at least 1 but not more than 3 predictors!")
  }

  if (np == 1) p <- plot_main_effect(object, predictors, ...)
  if (np == 2) p <- plot_twoway_effects(object, predictors, ...)
  if (np == 3) p <- plot_threeway_effects(object, predictors, ...)

  p
}

#' Plot glex Variable Importances
#'
#' @param object Object of class `glex_vi`, see [`glex_vi()`].
#' @param by_degree (`logical(1): FALSE`) Optionally sum values by degree of interaction, resulting in one contribution score
#'  for all main effects, all second-order interactions, etc.
#' @param scale (`"absolute"`) Plot average absolute contributions (default) or the same value but scaled by the
#' average prediction (`"relative"`).
#' @param threshold (`numeric(1)`: 0) Optional threshold to filter output to include only importance scores greater
#'   than this value. Refers to the chosen `scale`.
#' @param max_interaction (`integer(1): NULL`) Optionally filter plot to show terms up to the specified
#' degree of interaction. Similar to `threshold`, all other terms will be aggregated under a
#' `"Remaining terms"` label.
#' @param ... (Unused)
#'
#' @return A [`ggplot`][ggplot2::ggplot] object.
#' @export
#' @seealso [glex_vi]
#' @family Visualization functions
autoplot.glex_vi <- function(
    object, by_degree = FALSE,
    threshold = 0, max_interaction = NULL,
    scale = "absolute",
    ...
    ) {

  checkmate::assert_flag(by_degree)
  checkmate::assert_number(threshold, lower = 0, finite = TRUE)
  checkmate::assert_number(max_interaction, lower = 1, upper = max(object$degree), null.ok = TRUE)
  if (is.null(max_interaction)) max_interaction <- max(object$degree)
  checkmate::assert_subset(scale, choices = c("absolute", "relative"))

  object <- data.table::copy(object)
  by_what <- ifelse(by_degree, "Degree", "Term")
  score <- switch(scale, absolute = "m", relative = "m_rel")

  # FIXME: data.table NSE stuff
  m <- m_rel <- degree <- NULL

  aggr_degree <- function(x) {
     if (length(x) == 0) return(NULL)
     rng <- range(x)
     rng <- rng[!duplicated(rng)]
     paste(rng, collapse = "-")
  }

  # If max_interaction or threshold would select any observations, we summarize accordingly
  if (any(object$degree > max_interaction | abs(object[[score]]) <= threshold)) {

    # Sanity check that we're aggregating correctly by keeping track of the sums
    old_sum <- sum(object[[score]])

    # Keep rows not above max_interaction and scores above threshold
    keep <- object[degree <= max_interaction & abs(object[[score]]) > threshold, ]

    # Depending on whether we have a "class" col in multiclass, we need to summarize by that
    # I thought there was a neater way but by = NULL is not a great idea it turns out.
    if (is.null(object[["class"]])) {
      reduced <- object[degree > max_interaction | abs(object[[score]]) <= threshold,
                        list(m = sum(m), m_rel = sum(m_rel), degree = aggr_degree(degree), term = "Remaining terms")]
    } else {
      reduced <- object[degree > max_interaction | abs(object[[score]]) <= threshold,
                        list(m = sum(m), m_rel = sum(m_rel), degree = aggr_degree(degree), term = "Remaining terms"),
                        by = "class"]
    }

    # Combine again
    object <- rbind(keep, reduced)

    # Aforementioned sanity check: Sums should not differ aside from rounding error
    new_sum <- sum(object[[score]])
    checkmate::assert_number(abs(old_sum - new_sum), upper = 1e-13)
  }

  # Ugly hack to sort degree labels with the aggregated term (e.g. "2-4") last
  # Degree should be a factor for discrete color scale anyway.
  object[, degree := factor(degree, levels = unique(degree)[order(nchar(unique(degree)))])]
  if (by_what == "Degree") {

    if (is.null(object[["class"]])) {
      aggr <- object[, list(m = sum(m), m_rel = sum(m_rel)), by = "degree"]
    } else {
      aggr <- object[, list(m = sum(m), m_rel = sum(m_rel)), by = c("degree", "class")]
    }

    p <- ggplot(aggr)
    p <- p + aes(y = stats::reorder(.data[["degree"]], .data[[score]]))
    p <- p + guides(fill = "none")

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

  if (!is.null(object[["class"]])) {
    p <- p + facet_wrap(vars(.data[["class"]]), labeller = label_both)
  }

  p + aes(x = .data[[score]], fill = .data[["degree"]]) +
    geom_col(linewidth = 0, alpha = .8) +
    coord_cartesian(xlim = c(0, NA), expand = FALSE) +
    scale_fill_viridis_d(direction = -1, end = .95) +
    labs(
      title = sprintf("glex Variable Importances by %s", by_what),
      y = NULL, x = x_lab,
      fill = "Degree of Interaction"
    ) +
    theme_glex(grid_x = FALSE, grid_y = TRUE) +
    theme(axis.ticks.y = element_blank())
}
