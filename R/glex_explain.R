#' Explain a single prediction
#'
#' Plots the prediction components for a single observation, identified by the row number in the dataset used
#' with `glex()`.
#' Since the resulting plot can be quite busy due to potentially large amounts of elements, it is highly
#' recommended to use `predictors`, `max_interaction`, or `threshold` to restrict the number of
#' elements in the plot.
#'
#' @param object Object of class [`glex`] containing prediction components and data to be explained.
#' @param id (`integer(1)`) Row ID of the observation to be explained in `object$x`.
#' @param threshold (`numeric(1): 0`) Threshold to filter output by in case of many negligible effects.
#' @param max_interaction (`integer(1): NULL`) Optionally filter plot to show terms up to the specified
#' degree of interaction. Similar to `threshold`, all other terms will be aggregated under a
#' `"Remaining terms"` label.
#' @param predictors (`character: NULL`) Vector of column names in `$x` to restrict plot to.
#' @param class (`character: NULL`) For multiclass targets, specifies the target class to limit output.
#' @param barheight (`numeric(1): 0.5`) Relative height of horizontal bars.
#' Preferred value may depend on the number of vertical elements, hence it may be necessary to adjust
#' this value as needed.
#'
#' @return A [ggplot][ggplot2::ggplot] object.
# Invisibly: A `list` with elements
# * `components`: A [`data.table`] of the prediction components scaled by their degree of interaction,
#   grouped by their associated reference term.
# * `shap`: Similar to `components` but with the SHAP values, calculated as the sum of prediction
#   components.
# * `plot`: A [ggplot][ggplot2::ggplot] object.
#'
#' @importFrom scico scale_colour_scico_d
#' @importFrom utils head
#' @export
#' @family Visualization functions
#'
#' @examples
#' set.seed(1)
#' # Random Planted Forest -----
#' if (requireNamespace("randomPlantedForest", quietly = TRUE)) {
#' library(randomPlantedForest)
#'
#' rp <- rpf(mpg ~ ., data = mtcars[1:26, ], max_interaction = 2)
#'
#' glex_rpf <- glex(rp, mtcars[27:32, ])
#'
#' glex_explain(glex_rpf, id = 3, predictors = "hp", threshold = 0.01)
#' }
glex_explain <- function(
    object, id,
    threshold = 0, max_interaction = NULL,
    predictors = NULL,
    class = NULL,
    barheight = 0.5
) {

  checkmate::assert_class(object, "glex")
  checkmate::assert_int(id, lower = 1, upper = nrow(object$x))
  checkmate::assert_int(max_interaction, lower = 1, null.ok = TRUE)
  checkmate::assert_number(threshold, lower = 0)
  checkmate::assert_subset(predictors, names(object$x), empty.ok = TRUE)
  checkmate::assert_subset(class, object$target_levels, empty.ok = TRUE)

  # data.table NSE warnings
  .id <- m <- predsum <- term <- degree <- m_scaled <- NULL
  term_int <- xleft <- xright <- reference_term <- shap <- x <- NULL

  m_long <- melt_m(object$m, object$target_levels)

  mlong_id <- m_long[m_long[[".id"]] == id, ]
  mlong_id[, .id := NULL]
  mlong_id <- mlong_id[abs(mlong_id[["m"]]) > 0, ]

  # store away the intercept because we need that a bunch
  avgpred <- object$intercept

  # Get final prediction
  if (is.null(object$target_levels)) {
    pred <- sum(mlong_id[["m"]]) + avgpred
    pred <- format(pred, digits = 3, scientific = 4, justify = "none")
  } else {
    # Multiclass gets by-class pred
    pred <- mlong_id[, list(predsum = sum(m) + avgpred), by = "class"]
    pred <- as.character(pred[which.max(predsum), "class"][[1]])
  }

  mlong_id[, degree := get_degree(term)]
  mlong_id[, m_scaled := m / degree]
  mlong_id[, m := NULL]

  # Get observed values of selected predictors
  x_subset <- object$x[id, ]

  if (!is.null(predictors)) {
    x_subset <- x_subset[, predictors, with = FALSE]
  }

  # Iterate over all predictors and keep them as "reference term", so all main- and interaction effects can be
  # collected under one reference term, e.g. "x1" and "x1:x2" belong to x1, but "x1:x2" also belongs to x2
  xdf <- data.table::rbindlist(lapply(names(x_subset), function(main_term) {
    # get all terms associated with the current reference term
    mtemp <- mlong_id[find_term_matches(main_term, term), ]

    # In non-multiclass setting we need to create a "class" column so that following grouped ops work,
    # and having a dummy column is probably easier than having multiple if-else'd operations depending
    # on whether there's a class column I guess
    if (!("class" %in% names(mtemp))) {
      mtemp[, class := 1L]
    }

    if (threshold > 0) {
      mtemp <- rbind(
        mtemp[abs(mtemp[["m_scaled"]]) > threshold, ],
        mtemp[abs(mtemp[["m_scaled"]]) <= threshold,
              list(m_scaled = sum(m_scaled), term = "Remaining terms", degree = 100), by = "class"]
      )
    }

    if (!is.null(max_interaction)) {
      mtemp <- rbind(
        mtemp[degree <= max_interaction, ],
        mtemp[degree > max_interaction,
              list(m_scaled = sum(m_scaled), term = "Remaining terms", degree = 100), by = "class"]
      )
    }

    # term_int serves as y-axis which needs to be an integer as that makes everything easier regarding y-positioning
    # order by absolute contribution
    mtemp[, term_int := order(degree, m_scaled), by = "class"]
    setorder(mtemp, term_int)

    # left and right x position of the rectangles for each contribution term. it makes sense I swear.
    # crucial part is that the intercept (avgpred) serves as reference/anchor point
    mtemp[, xright := c(head(avgpred + cumsum(m_scaled), -1), avgpred + sum(m_scaled)), by = "class"]
    mtemp[, xleft := c(avgpred, head(xright, -1)), by = "class"]

    mtemp[, reference_term := main_term]
    mtemp
  }))

  # We need a lookup-table for the facet labels. Names are the predictors, values are the observed predictor values
  # Has to be a character vector (not a list afaict), so numeric + factor/character features don't mix nicely.
  xnames <- names(x_subset)
  xtemp <- as.character(format(x_subset, digits = 3))
  xtemp <- unlist(Map(paste, xnames, xtemp, sep = " = "))
  names(xtemp) <- xnames

  # helper df of shap values to plot them in as separate rectangles, at last (lowest) position (determined by term_int)
  xshap <- xdf[, list(shap = sum(m_scaled), term_int = max(term_int) + 1),
               by = c("reference_term", "class")]
  xshap[, xleft := avgpred + shap]
  xshap[, xright := avgpred]

  # Subset to selected class, could be of length > 1
  if (!is.null(class)) {
    # Pre-calculated indices for subsetting to avoid DT NSE name clashes
    idx_class <- which(xdf[["class"]] %in% class)
    idx_class_shap <- which(xshap[["class"]] %in% class)

    xdf <- xdf[idx_class, ]
    xshap <- xshap[idx_class_shap, ]
  }

  p <- ggplot(xdf, aes(y = term_int, fill = as.character(sign(m_scaled))))

  if (is.null(object$target_levels)) {
    p <- p + facet_wrap(
      vars(reference_term),
      scales = "free_x", labeller = labeller(reference_term = xtemp)
    )
  } else {
    p <- p + facet_wrap(
      vars(.data[["reference_term"]], .data[["class"]]),
      scales = "free_x", labeller = labeller(reference_term = xtemp, class = label_both)
    )
  }

  # geoms
  p <- p +
    geom_vline(xintercept = avgpred, linetype = "6161") +
    # Draw contribution bars
    geom_segment(aes(
      y = term_int - barheight/2, yend = term_int + 1 + barheight/2,
      x = xright, xend = after_stat(x),
      color = as.character(sign(m_scaled))
    ), linetype = "solid") +
    geom_rect(
      aes(
        xmin = xleft, xmax = xright,
        ymin = term_int - barheight/2, ymax = term_int + barheight/2
      ),
      alpha = .75
    ) +
    # Draw SHAP bar below with separate df
    geom_rect(
      data = xshap,
      aes(
        fill = as.character(sign(shap)),
        xmin = xleft, xmax = xright,
        ymin = term_int - barheight/2, ymax = term_int + barheight/2
      ),
      alpha = .75
    ) +
    # Label SHAP values below other bars
    geom_label(
      data = xshap, aes(
        label = sprintf("SHAP: %s", format(shap, digits = 2)),
        x = xright, fill = NULL,
        hjust = pmin(1.05, 0.95 + sign(shap))
      )
    ) +
    # Label contribution values
    geom_label(
      aes(x = xleft,
        label = sprintf("%s: %s", term, round(m_scaled, 2)),
        hjust = pmin(1.05, 0.95 + sign(m_scaled))),
      color = "white", alpha = .75
    )

  # scales / coords
  p <- p +
    scico::scale_fill_scico_d(
      palette = "vikO", begin = .75, end = .25,
      guide = "none",
      aesthetics = c("color", "fill")
    ) +
    scale_x_continuous(expand = expansion(mult = .5)) +
    scale_y_reverse(labels = NULL) +
    coord_cartesian(clip = "off")

  # Labels and theming
  p <- p +
    labs(
      title = sprintf("Decomposition for ID %d with predicted value %s", id, pred),
      subtitle = sprintf("Centered around average prediction: %1.2f", avgpred),
      x = "Average prediction +/- m",
      y = NULL
    ) +
    # theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(colour = "#222222"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(hjust = 0.5, size = 13, face = "bold"),
      panel.spacing.x = unit(2, "lines"),
      plot.subtitle = element_text(colour = "#202020")
    )

  # print(p)
  # invisible(list(components = xdf, shap = xshap, plot = p))
  p +
    theme_glex()
}

