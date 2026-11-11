#' @rdname plot_components
#' @param measure (`character(1) | NULL: NULL`) Without `predictors`: the importance of each component, given in its
#'   title and ordering the plots, computed from the components at `x` (see [glex_vi()]):
#'   * `"variance"`: its mean square as a share of the variance of the prediction at `x`, in percent;
#'   * `"mean_abs"`: its mean absolute value, in units of the prediction (`y` for regression).
#'
#'   `NULL` uses `"variance"` for a Random Planted Forest from `rpf()` of randomPlantedForest 0.6 or later (class
#'   `rpf_pruned`) and `"mean_abs"` otherwise.
#' @param max_components (`integer(1) | NULL: NULL`) Without `predictors`: the most components to draw, the most
#'   important first. `NULL` draws all components of a model of class `rpf_pruned` and at most 12 otherwise.
#' @param ncol (`integer(1) | NULL: NULL`) Without `predictors`: the number of columns of main effects and pairs. A
#'   component with several panels spans a row of its own. `NULL` uses up to 4.
#' @export
#' @family Visualization functions
#' @details
#' `autoplot()` without `predictors` draws the model's components in one figure (with the patchwork package), the most
#' important first by `measure`, each titled by its name and importance, e.g. `x2:x3 (24%)`: main effects and two-way
#' interactions for any model, and for a model of class `rpf_pruned` (what `rpf()` returns from randomPlantedForest
#' 0.6 on) all of its components, including those of three or more predictors.
#'
#' **Models of class `rpf_pruned`.** Such a model has few components, each stored in the form it predicts with. The
#' plots show each component as it enters the prediction:
#' * a main effect as a step curve through its values at `x`;
#' * a two-way interaction as a heat map, evaluated on a grid of the values of its predictors in `x`: at most
#'   `resolution` cells along each predictor, the distinct values if there are at most `resolution`, otherwise the
#'   values at the quantiles `(i - 0.5) / resolution`, each cell reaching halfway to its neighbours (a factor: one cell
#'   per level);
#' * a component of three or more predictors as heat maps over two of its predictors, one per cell of the others (the
#'   cells the model stores for it), each labelled by its cell, on one colour scale. The heat maps are exact in the
#'   other predictors: the component is constant on each cell. `plot_threeway_effects()` draws them for three
#'   predictors, `autoplot()` for any number; the order of `predictors` does not matter. (A model from an earlier
#'   randomPlantedForest that does not store the cells gets one heat map, the other predictors at their values in the
#'   first row of `x`, with a message.)
#'
#' For multiclass models every plot has one panel (or set of heat maps) per class. The grids need the model, which the
#' glex object keeps: a model saved to a file and restored without `randomPlantedForest::rpf_unmarshal()` can't be
#' evaluated.
autoplot.glex <- function(
  object,
  predictors = NULL,
  ...,
  measure = NULL,
  max_components = NULL,
  ncol = NULL,
  resolution = 100L
) {
  checkmate::assert_choice(measure, c("mean_abs", "variance"), null.ok = TRUE)
  checkmate::assert_int(resolution, lower = 2)
  if (is.null(predictors)) {
    return(plot_glex_overview(
      object,
      measure = measure,
      max_components = max_components,
      ncol = ncol,
      resolution = resolution,
      ...
    ))
  }
  np <- length(predictors)

  if (np > 3 && is_pruned_glex(object)) {
    checkmate::assert_character(predictors, unique = TRUE)
    checkmate::assert_subset(predictors, names(object$x))
    return(plot_pruned_panels(object, predictors, resolution = resolution))
  }
  if (np < 1 | np > 3) {
    stop("You need to specify at least 1 but not more than 3 predictors!")
  }

  if (np == 1) {
    p <- plot_main_effect(object, predictors, ...)
  }
  if (np == 2) {
    p <- plot_twoway_effects(object, predictors, resolution = resolution, ...)
  }
  if (np == 3) {
    p <- plot_threeway_effects(object, predictors, resolution = resolution, ...)
  }

  p
}

#' Plot glex Variable Importances
#'
#' @param object Object of class `glex_vi`, see [`glex_vi()`].
#' @param by_degree (`logical(1): FALSE`) Optionally sum values by degree of interaction, resulting in one contribution score
#'  for all main effects, all second-order interactions, etc.
#' @param scale (`"absolute"`) Plot `m` (default) or `m_rel` (`"relative"`) of [glex_vi()]: for its default
#' `measure = "mean_abs"`, the average absolute contributions or the same value scaled by the average prediction; for
#' `"variance"`, the mean square or its share of the variance of the prediction; for `"share"`, the pruning's share
#' either way.
#' @param threshold (`numeric(1)`: 0) Optional threshold to filter output to include only importance scores greater
#'   than this value. Refers to the chosen `scale`.
#' @param max_interaction (`integer(1): NULL`) Optionally filter plot to show terms up to the specified
#' degree of interaction. Similar to `threshold`, all other terms will be aggregated under a
#' `"Remaining terms"` label.
#' @param ... (Unused)
#'
#' @returns A [`ggplot`][ggplot2::ggplot] object.
#' @export
#' @seealso [glex_vi]
#' @family Visualization functions
autoplot.glex_vi <- function(
  object,
  by_degree = FALSE,
  threshold = 0,
  max_interaction = NULL,
  scale = "absolute",
  ...
) {
  checkmate::assert_flag(by_degree)
  checkmate::assert_number(threshold, lower = 0, finite = TRUE)
  checkmate::assert_number(
    max_interaction,
    lower = 1,
    upper = max(object$degree),
    null.ok = TRUE
  )
  if (is.null(max_interaction)) {
    max_interaction <- max(object$degree)
  }
  checkmate::assert_subset(scale, choices = c("absolute", "relative"))

  measure <- attr(object, "measure")
  if (is.null(measure)) {
    measure <- "mean_abs"
  }
  object <- data.table::copy(object)
  by_what <- ifelse(by_degree, "Degree", "Term")
  score <- switch(scale, absolute = "m", relative = "m_rel")

  m <- m_rel <- degree <- NULL

  aggr_degree <- function(x) {
    if (length(x) == 0) {
      return(NULL)
    }
    rng <- range(x)
    rng <- rng[!duplicated(rng)]
    paste(rng, collapse = "-")
  }

  # If max_interaction or threshold would select any observations, we summarize accordingly
  if (any(object$degree > max_interaction | abs(object[[score]]) <= threshold)) {
    # Sanity check that we're aggregating correctly by keeping track of the sums
    old_sum <- sum(object[[score]])

    # Keep rows not above max_interaction and scores above threshold
    keep <- object[
      degree <= max_interaction & abs(object[[score]]) > threshold,
    ]

    # Depending on whether we have a "class" col in multiclass, we need to summarize by that
    # I thought there was a neater way but by = NULL is not a great idea it turns out.
    if (is.null(object[["class"]])) {
      reduced <- object[
        degree > max_interaction | abs(object[[score]]) <= threshold,
        list(
          m = sum(m),
          m_rel = sum(m_rel),
          degree = aggr_degree(degree),
          term = "Remaining terms"
        )
      ]
    } else {
      reduced <- object[
        degree > max_interaction | abs(object[[score]]) <= threshold,
        list(
          m = sum(m),
          m_rel = sum(m_rel),
          degree = aggr_degree(degree),
          term = "Remaining terms"
        ),
        by = "class"
      ]
    }

    # Combine again
    object <- rbind(keep, reduced)

    # Aforementioned sanity check: Sums should not differ aside from rounding error
    new_sum <- sum(object[[score]])
    checkmate::assert_number(
      abs(old_sum - new_sum),
      upper = 1e-13 * max(1, abs(old_sum))
    )
  }

  # Ugly hack to sort degree labels with the aggregated term (e.g. "2-4") last
  # Degree should be a factor for discrete color scale anyway.
  object[,
    degree := factor(
      degree,
      levels = unique(degree)[order(nchar(unique(degree)))]
    )
  ]
  if (by_what == "Degree") {
    if (is.null(object[["class"]])) {
      aggr <- object[, list(m = sum(m), m_rel = sum(m_rel)), by = "degree"]
    } else {
      aggr <- object[,
        list(m = sum(m), m_rel = sum(m_rel)),
        by = c("degree", "class")
      ]
    }

    p <- ggplot(aggr)
    p <- p + aes(y = stats::reorder(.data[["degree"]], .data[[score]]))
    p <- p + guides(fill = "none")

    x_lab <- vi_label(measure, score, sum = TRUE)
  }

  if (by_what == "Term") {
    p <- ggplot(object)
    p <- p + aes(y = stats::reorder(.data[["term"]], .data[[score]]))

    x_lab <- vi_label(measure, score)
  }

  if (!is.null(object[["class"]])) {
    p <- p + facet_wrap(vars(.data[["class"]]), labeller = label_both)
  }

  p +
    aes(x = .data[[score]], fill = .data[["degree"]]) +
    geom_col(linewidth = 0, alpha = .8) +
    coord_cartesian(xlim = c(0, NA), expand = FALSE) +
    scale_fill_viridis_d(direction = -1, end = .95) +
    labs(
      title = sprintf("glex Variable Importances by %s", by_what),
      y = NULL,
      x = x_lab,
      fill = "Degree of Interaction"
    ) +
    theme_glex(grid_x = FALSE, grid_y = TRUE) +
    theme(axis.ticks.y = element_blank())
}

#' Axis label of a glex_vi plot by measure and score column
#' @keywords internal
#' @noRd
vi_label <- function(measure, score, sum = FALSE) {
  if (measure == "mean_abs" && sum) {
    return(switch(
      score,
      m = "Sum of Average Absolute Contributions",
      m_rel = "Sum of Average Absolute Contributions\nRelative to Average Prediction"
    ))
  }
  label <- switch(
    measure,
    mean_abs = switch(
      score,
      m = "Average Absolute Contribution",
      m_rel = "Average Absolute Contribution\nRelative to Average Prediction"
    ),
    variance = switch(
      score,
      m = "Mean Square of Contribution",
      m_rel = "Share of the Variance of the Prediction"
    ),
    share = "Share of the Variance of the Prediction\n(Training Rows)"
  )
  if (sum) paste("Sum of", label) else label
}
