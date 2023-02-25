#' Variable Importance for Main and Interaction Terms
#'
#' @param object Object of class `glex`.
#' @param threshold (`numeric(1)`) Optional threshold to filter output to include only importance scores greater
#'   than this value. Refers to percentage of average absolute `m` divided by average prediction (`intercept`).
#' @param ... (Unused)
#'
#' @return A data.table
#' @export
#'
#' @examples
#' # Random Planted Forest -----
#' if (requireNamespace("randomPlantedForest", quietly = TRUE)) {
#' library(randomPlantedForest)
#'
#' rp <- rpf(mpg ~ ., data = mtcars[1:26, ], max_interaction = 2)
#'
#' glex_rpf <- glex(rp, mtcars[27:32, ])
#'
#' # All terms
#' glex_vi(glex_rpf)
#'
#' # Only terms with a relative contribution greater or equal 0.1% of the average prediction
#' (vi <- glex_vi(glex_rpf, threshold = 0.1))
#'
#' library(ggplot2)
#' autoplot(vi)
#' autoplot(vi, by_degree = TRUE)
#' }
glex_vi <- function(object, threshold = 0, ...) {
  checkmate::assert_class(object, classes = "glex")
  checkmate::assert_number(threshold, lower = 0, finite = TRUE)

  # FIXME: data.table NSE warnings
  term <- term_list <- degree <- m <- m_rel <- ..intercept <- NULL

  xcomp <- data.table::copy(object$m)
  intercept <- object$intercept
  pred <- rowSums(xcomp) + intercept

  xcomp[, ".id" := seq_len(.N)]

  # longify for extraction
  m_long <- melt(xcomp, id.vars = ".id", value.name = "m", variable.name = "term", variable.factor = FALSE)

  m_aggr <- m_long[, list(m = mean(abs(m))), by = "term"]
  m_aggr[, term_list := lapply(term, function(x) unlist(strsplit(x, ":")))]
  m_aggr[, degree := vapply(term_list, length, integer(1))]
  m_aggr[, m_rel := 100 * (m / ..intercept)]

  data.table::setorder(m_aggr, -m)
  data.table::setcolorder(m_aggr, c("degree", "term", "term_list", "m", "m_rel"))
  class(m_aggr) <- c("glex_vi", class(m_aggr))
  m_aggr[m_rel >= threshold]
}


#' Plot glex Variable Importances
#'
#' @param object Object of class `glex_vi`, see [`glex_vi()`].
#' @param by_degree (`FALSE`) Optionally sum values by degree of interaction, resulting in one contribution score
#'  for all main effects, all second-order interactions, etc.
#' @param ... (Unused)
#'
#' @return A `ggplot2` object.
#' @export
autoplot.glex_vi <- function(object, by_degree = FALSE, ...) {

  checkmate::assert_flag(by_degree)
  object <- data.table::copy(object)
  by_what <- ifelse(by_degree, "Degree", "Term")

  # FIXME: data.table NSE stuff
  m <- NULL

  if (by_degree) {
    p <- ggplot(object[, list(m = sum(m)), by = "degree"],
            aes(y = stats::reorder(.data[["degree"]], .data[["m"]])))
    x_lab <- "Sum of Average Absolute Contributions"
  } else {
    p <- ggplot(object, aes(y = stats::reorder(.data[["term"]], .data[["m"]])))
    x_lab <- "Average Absolute Contribution"
  }

  p + aes(x = .data[["m"]], fill = factor(.data[["degree"]])) +
    geom_col(color = "black", linewidth = .1) +
    scale_fill_viridis_d(direction = -1, end = .95) +
    theme(legend.position = "bottom") +
    labs(
      title = sprintf("Average Absolute Prediction Contribution by %s", by_what),
      y = by_what, x = x_lab,
      fill = "Degree of Interaction"
    )
}
