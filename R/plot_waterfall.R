#' Waterfall plot of a single prediction
#'
#' Shows how the prediction for one observation is built from the components of
#' the decomposition: starting at the intercept E\[f\], each bar adds one main or
#' interaction term \eqn{m_S} until the prediction f(x) is reached. Unlike
#' [plot_shap_decomposition()], which splits interaction terms among their
#' features, every term is its own bar.
#'
#' @param object Object of class [`glex`].
#' @param id (`integer(1)`) Row of `object$x` to explain.
#' @param threshold (`numeric(1): 0`) Terms with an absolute value at or below
#'   this are aggregated into one "other terms" bar.
#' @param max_terms (`integer(1): 10`) Maximum number of terms shown
#'   individually, keeping the largest in absolute value. The rest are aggregated.
#' @param max_interaction (`integer(1): NULL`) Terms of a higher degree are
#'   aggregated.
#' @param class (`character(1): NULL`) For multiclass objects, the class whose
#'   prediction is shown. Required for multiclass objects.
#'
#' @details
#' Bars of interaction terms are more transparent the higher their degree. For
#' constrained decompositions (see [glex()]), what the dropped terms account for
#' is shown as a final "remainder (constrained)" bar, so the plot still ends at
#' the model's prediction.
#'
#' @returns A [ggplot][ggplot2::ggplot] object.
#' @export
#' @family Visualization functions
#' @examplesIf requireNamespace("randomPlantedForest", quietly = TRUE)
#' library(randomPlantedForest)
#' set.seed(1)
#' rp <- rpf(mpg ~ ., data = mtcars[1:26, ], max_interaction = 2)
#' gl <- glex(rp, mtcars[27:32, ])
#'
#' plot_waterfall(gl, id = 3)
#' plot_waterfall(gl, id = 3, max_terms = 5)
plot_waterfall <- function(
  object,
  id,
  threshold = 0,
  max_terms = 10L,
  max_interaction = NULL,
  class = NULL
) {
  et <- explain_terms(object, id, class, threshold, max_terms, max_interaction)
  d <- et$terms
  n <- nrow(d)
  barheight <- 0.7

  d$row <- seq_len(n)
  d$sign <- as.character(sign(d$m))
  d$alpha <- degree_alpha(d$degree)
  d$value <- format_contribution(d$m)
  d$xlab <- ifelse(d$m >= 0, pmax(d$start, d$end), pmin(d$start, d$end))
  d$hjust <- ifelse(d$m >= 0, -0.15, 1.15)

  connectors <- data.frame(
    x = d$end[-n],
    y = d$row[-n] + barheight / 2,
    yend = d$row[-1] - barheight / 2
  )
  xrange <- diff(range(c(d$start, d$end, et$intercept, et$prediction)))

  ggplot(d) +
    geom_vline(
      xintercept = c(et$intercept, et$prediction),
      linetype = "dashed",
      colour = "grey40"
    ) +
    geom_segment(
      data = connectors,
      aes(x = .data$x, xend = .data$x, y = .data$y, yend = .data$yend),
      colour = "grey40",
      linewidth = 0.3
    ) +
    geom_rect(
      aes(
        xmin = .data$start,
        xmax = .data$end,
        ymin = .data$row - barheight / 2,
        ymax = .data$row + barheight / 2,
        fill = .data$sign,
        alpha = .data$alpha
      )
    ) +
    geom_text(
      aes(x = .data$xlab, y = .data$row, label = .data$value, hjust = .data$hjust),
      size = 3.5
    ) +
    scale_fill_manual(values = sign_colors(), guide = "none") +
    scale_alpha_identity() +
    scale_y_reverse(breaks = d$row, labels = d$label, expand = expansion(add = 0.6)) +
    scale_x_continuous(
      expand = label_expansion(d$value),
      sec.axis = reference_axis(et, xrange)
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title = prediction_title(id, et, class),
      subtitle = terms_subtitle(et),
      x = "Prediction",
      y = NULL
    ) +
    theme_glex(grid_x = FALSE, grid_y = TRUE)
}
