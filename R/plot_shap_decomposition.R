#' Plot how a feature's SHAP value is assembled from components
#'
#' For one observation, shows the components involving each selected feature,
#' starting at the intercept E\[f\]. Interaction terms are split evenly among
#' their features, so each feature's bars sum to its SHAP value, shown in the
#' bottom row. To see the whole prediction instead, use [plot_waterfall()] or
#' [plot_force()].
#'
#' The plot can get busy; use `predictors`, `max_interaction`, or `threshold` to
#' restrict it.
#'
#' @param object Object of class [`glex`] containing prediction components and
#'   data to be explained.
#' @param id (`integer(1)`) Row ID of the observation to be explained in `object$x`.
#' @param threshold (`numeric(1): 0`) Terms whose contribution is at or below
#'   this are aggregated under a `"Remaining terms"` label.
#' @param max_interaction (`integer(1): NULL`) Terms of a higher degree are
#'   aggregated under the `"Remaining terms"` label.
#' @param predictors (`character: NULL`) Vector of column names in `$x` to
#'   restrict the plot to.
#' @param class (`character: NULL`) For multiclass targets, the target classes to
#'   show.
#' @param barheight (`numeric(1): 0.5`) Relative height of the bars.
#' @param ... Arguments passed to `plot_shap_decomposition()`.
#'
#' @returns A [ggplot][ggplot2::ggplot] object.
#' @export
#' @family Visualization functions
#' @examplesIf requireNamespace("randomPlantedForest", quietly = TRUE)
#' set.seed(1)
#' library(randomPlantedForest)
#' rp <- rpf(mpg ~ ., data = mtcars[1:26, ], max_interaction = 2)
#' glex_rpf <- glex(rp, mtcars[27:32, ])
#'
#' plot_shap_decomposition(glex_rpf, id = 3, predictors = "hp", threshold = 0.01)
plot_shap_decomposition <- function(
  object,
  id,
  threshold = 0,
  max_interaction = NULL,
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

  # data.table NSE
  .id <- m <- predsum <- term <- degree <- m_scaled <- NULL
  xleft <- xright <- reference_term <- kind <- pos <- NULL
  ykey <- value <- xlab <- hjust <- ynext <- NULL

  m_long <- melt_m(object$m, object$target_levels)
  mlong_id <- m_long[m_long[[".id"]] == id, ]
  mlong_id[, .id := NULL]
  mlong_id <- mlong_id[abs(mlong_id[["m"]]) > 0, ]
  avgpred <- object$intercept

  if (is.null(object$target_levels)) {
    pred <- format(sum(mlong_id[["m"]]) + avgpred, digits = 3)
    # dummy class so the grouped operations below need no branching
    mlong_id[, class := "1"]
  } else {
    pred <- mlong_id[, list(predsum = sum(m) + avgpred), by = "class"]
    pred <- as.character(pred[which.max(predsum), "class"][[1]])
    mlong_id[, class := as.character(class)]
  }

  mlong_id[, degree := get_degree(term)]
  mlong_id[, m_scaled := m / degree]
  mlong_id[, m := NULL]

  x_subset <- object$x[id, ]
  if (!is.null(predictors)) {
    x_subset <- x_subset[, predictors, with = FALSE]
  }

  # Each feature collects its main effect and all interactions it is part of
  xdf <- data.table::rbindlist(
    lapply(names(x_subset), function(main_term) {
      mtemp <- mlong_id[find_term_matches(main_term, term), ]
      rest <- abs(mtemp$m_scaled) <= threshold
      if (!is.null(max_interaction)) {
        rest <- rest | mtemp$degree > max_interaction
      }
      kept <- mtemp[!rest, ][order(class, -abs(m_scaled))]
      kept[, kind := "term"]
      remaining <- mtemp[
        rest,
        list(m_scaled = sum(m_scaled), term = "Remaining terms", kind = "remaining"),
        by = "class"
      ]
      out <- rbind(kept[, list(class, term, m_scaled, kind)], remaining, use.names = TRUE)
      out[, reference_term := main_term]
      out
    }),
    use.names = TRUE
  )
  xdf[, xright := avgpred + cumsum(m_scaled), by = c("reference_term", "class")]
  xdf[, xleft := xright - m_scaled]

  # Objects from earlier glex versions have no `$shap`; constrained ones hold NA
  shap_valid <- !is.null(object$shap) && length(object$constrained) == 0
  if (shap_valid) {
    shap_long <- melt_m(object$shap, object$target_levels)
    shap_id <- shap_long[shap_long[[".id"]] == id & shap_long[["term"]] %in% names(x_subset), ]
    shap_id <- shap_id[, list(
      reference_term = term,
      class = if (is.null(object$target_levels)) "1" else as.character(class),
      term = "SHAP",
      m_scaled = m,
      kind = "shap",
      xleft = avgpred,
      xright = avgpred + m
    )]
    xdf <- rbind(xdf, shap_id, use.names = TRUE)
  }

  if (!is.null(class)) {
    # Index outside `[` because data.table would resolve `class` to the column
    idx_class <- which(xdf[["class"]] %in% class)
    xdf <- xdf[idx_class, ]
  }

  # Discrete y per facet: rows in plotting order, the first level is drawn at the bottom
  xdf[, pos := seq_len(.N), by = c("reference_term", "class")]
  xdf[, ykey := paste(reference_term, class, pos, sep = "\r")]
  xdf[, ykey := factor(ykey, levels = ykey[order(-pos)])]
  xdf[, sign := as.character(sign(m_scaled))]
  xdf[, value := format_contribution(m_scaled)]
  xdf[, xlab := ifelse(m_scaled >= 0, pmax(xleft, xright), pmin(xleft, xright))]
  xdf[, hjust := ifelse(m_scaled >= 0, -0.15, 1.15)]

  connectors <- xdf[kind != "shap"]
  connectors[, ynext := data.table::shift(as.character(ykey), type = "lead"), by = c("reference_term", "class")]
  connectors <- connectors[!is.na(ynext)]
  connectors[, ynext := factor(ynext, levels = levels(xdf$ykey))]

  xnames <- names(x_subset)
  facet_labels <- stats::setNames(
    paste(xnames, vapply(x_subset, function(v) format_feature_value(v[[1]]), character(1)), sep = " = "),
    xnames
  )

  p <- ggplot(xdf)
  p <- if (is.null(object$target_levels)) {
    p + facet_wrap(vars(.data$reference_term), scales = "free", labeller = labeller(reference_term = facet_labels))
  } else {
    p +
      facet_wrap(
        vars(.data$reference_term, .data$class),
        scales = "free",
        labeller = labeller(reference_term = facet_labels, class = label_both)
      )
  }

  p <- p +
    geom_vline(xintercept = avgpred, linetype = "dashed", colour = "grey40") +
    geom_segment(
      data = connectors,
      aes(x = .data$xright, xend = .data$xright, y = .data$ykey, yend = .data$ynext),
      colour = "grey40",
      linewidth = 0.3
    ) +
    geom_tile(
      aes(
        x = (.data$xleft + .data$xright) / 2,
        width = abs(.data$m_scaled),
        y = .data$ykey,
        height = barheight,
        fill = .data$sign
      ),
      alpha = 0.85
    ) +
    geom_text(
      aes(x = .data$xlab, y = .data$ykey, label = .data$value, hjust = .data$hjust),
      size = 3.3
    )
  if (shap_valid) {
    p <- p + geom_hline(yintercept = 1.5, colour = "grey60", linewidth = 0.3)
  }

  p +
    scale_fill_manual(values = sign_colors(), guide = "none") +
    # Explicit order: the scale would otherwise learn it from the first layer, which lacks some rows
    scale_y_discrete(
      limits = function(present) intersect(levels(xdf$ykey), present),
      labels = stats::setNames(xdf$term, as.character(xdf$ykey))
    ) +
    scale_x_continuous(
      expand = label_expansion(xdf$value, n_panels = data.table::uniqueN(xdf, by = c("reference_term", "class")))
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title = sprintf("SHAP decomposition for observation %d, predicted value %s", id, pred),
      subtitle = sprintf(
        "Bars show m_S / |S|: interaction terms are split evenly among their features.\nStarting at E[f] = %s%s",
        format(avgpred, digits = 3),
        if (shap_valid) {
          ""
        } else {
          sprintf(
            "\nSHAP values omitted: %s",
            if (is.null(object$shap)) {
              "object has no $shap"
            } else {
              sprintf("decomposition constrained by %s", paste0("`", object$constrained, "`", collapse = " and "))
            }
          )
        }
      ),
      x = "Contribution (m_S / |S|)",
      y = NULL
    ) +
    theme_glex(grid_x = FALSE, grid_y = TRUE) +
    theme(
      strip.text = element_text(hjust = 0.5, size = 12, face = "bold"),
      panel.spacing.x = unit(2, "lines")
    )
}

#' @rdname plot_shap_decomposition
#' @export
glex_explain <- function(...) {
  lifecycle::deprecate_soft("0.8.0", "glex_explain()", "plot_shap_decomposition()")
  plot_shap_decomposition(...)
}
