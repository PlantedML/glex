#' Explain a single prediction
#'
#' Plots the prediction components for a single observation, identified by the row number in the dataset used
#' with `glex()`.
#'
#' @param object Object of class [`glex`] containing prediction components and data to be explained.
#' @param id (`integer(1)`) Row ID of the observation to be explained in `object$x`.
#' @param threshold (`numeric(1): 0`) Threshold to filter output by in case of many negligible effects.
#'
#' @return A [ggplot2::ggplot] object.
#' @importFrom scico scale_colour_scico_d
#' @export
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
#' glex_explain(glex_rpf, id = 3, threshold = 0.2)
#' }
glex_explain <- function(object, id, threshold = 0) {
  checkmate::assert_class(object, "glex")
  checkmate::assert_int(id, lower = 1, upper = nrow(object$x))
  checkmate::assert_number(threshold, lower = 0)

  # data.table NSE warnings
  m <- predsum <- NULL

  m_long <- melt_m(object$m, object$target_levels)
  m_long <- m_long[m_long[[".id"]] == id, ]
  x_subset <- object$x[id, ]

  # Get final prediction
  if (is.null(object$target_levels)) {
    pred <- sum(m_long[["m"]]) + object$intercept
    pred <- format(pred, scientific = 4, justify = "none")
  } else {
    # Multiclass gets by-class pred
    pred <- m_long[, list(predsum = sum(m) + object$intercept), by = "class"]
    pred <- as.character(pred[which.max(predsum), "class"][[1]])
  }

  m_long <- m_long[abs(m_long[["m"]]) > threshold, ]

  p <- ggplot(m_long, aes(
      y = stats::reorder(.data[["term"]], abs(.data[["m"]])),
      #x = .data[["value"]] + object$intercept,
      color = as.character(sign(.data[["m"]]))
    )) +
    #geom_point(shape = 21, size = 3) +
    geom_segment(
      x = object$intercept,
      aes(
        xend = .data[["m"]] + object$intercept,
        yend = after_stat(.data[["y"]])
      ),
      arrow = grid::arrow(angle = 45, type = "closed"),
      linewidth = 1,
      lineend = "butt",
      show.legend = FALSE
    ) +
    geom_vline(xintercept = object$intercept, linetype = "6161") +
    scale_y_discrete(labels = function(m) label_m_x(m, x_subset)) +
    #scale_color_manual(values = c(`TRUE` = "blue", `FALSE` = "red")) +
    scico::scale_colour_scico_d(palette = "vikO", begin = .75, end = .25) +
    labs(
      title = sprintf("ID %i with predicted value %s", id, pred),
      #subtitle = sprintf("Predicted value: %1.3f", pred),
      x = "Average Prediction +/- m"
    ) +
    theme(
      plot.title.position = "plot",
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(colour = "#222222", size = 11)
    )

  if (!is.null(object$target_levels)) {
    p <- p + facet_wrap(vars(.data[["class"]]), labeller = label_both)
  }

  p
}
