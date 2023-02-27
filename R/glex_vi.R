#' Variable Importance for Main and Interaction Terms
#'
#' @param object Object of class `glex`.
#' @param threshold (`numeric(1)`) Optional threshold to filter output to include only importance scores greater
#'   than this value. Refers to `m_rel`, the average absolute `m` divided by average prediction (`intercept`).
#' @param ... (Unused)
#'
#' @return A data.table with columns:
#' * `degree` (`integer`): Degree of interaction of the `term`, with `1`  being main effects, `2` being 2-degree interactions etc.
#' * `term` (`character`): Model term, e.g. main effect `x1` or interaction term `x1:x2`, `x1:x3:x5` etc.
#' * `term_list` (`list`): Same as `term` but as a list-column to enable filtering by specific variables without
#'   requiring to split by `:`.
#' * `m` (`numeric`): Average absolute contribution of `term`, see Details.
#' * `m_rel` (`numeric`): `m` but relative to the average prediction (`intercept` in `glex()` output).
#'
#' @export
#'
#' @details
#' The `m` reported here is the average absolute value of `m` as reported by `glex()`, aggregated by `term`:
#'
#' \deqn{\mathtt{m} = \frac{1}{n} \sum_{i = 1}^n |m| }
#'
#' In turn, `m_rel` rescales `m` by the average prediction of the model (\eqn{m_0}, `intercept` as reported by `glex()`):
#'
#' \deqn{\mathtt{m\_rel} = \frac{\mathtt{m}}{m_0}}
#'
#' @seealso autoplot.glex_vi
#' @examples
#' # Random Planted Forest -----
#' if (requireNamespace("randomPlantedForest", quietly = TRUE)) {
#' library(randomPlantedForest)
#'
#' rp <- rpf(mpg ~ ., data = mtcars[1:26, ], max_interaction = 4)
#'
#' glex_rpf <- glex(rp, mtcars[27:32, ])
#'
#' # All terms
#' vi_rpf <- glex_vi(glex_rpf)
#'
#' # Only terms with a relative contribution greater or equal 0.1% of the average prediction
#' vi_rpf_filtered <- glex_vi(glex_rpf, threshold = 0.001)
#'
#' library(ggplot2)
#' autoplot(vi_rpf_filtered)
#' autoplot(vi_rpf, by_degree = TRUE)
#' autoplot(vi_rpf, scale = "relative", threshold = 0.005)
#' }
#'
#' # xgboost -----
#' if (requireNamespace("xgboost", quietly = TRUE)) {
#' library(xgboost)
#' x <- as.matrix(mtcars[, -1])
#' y <- mtcars$mpg
#' xg <- xgboost(data = x[1:26, ], label = y[1:26],
#'               params = list(max_depth = 4, eta = .1),
#'               nrounds = 10, verbose = 0)
#' glex_xgb <- glex(xg, x[27:32, ])
#' vi_xgb <- glex_vi(glex_xgb)
#'
#' library(ggplot2)
#' autoplot(vi_xgb)
#' autoplot(vi_xgb, by_degree = TRUE)
#' autoplot(vi_xgb, scale = "relative")
#' }
glex_vi <- function(object, threshold = 0, ...) {
  checkmate::assert_class(object, classes = "glex")
  checkmate::assert_number(threshold, lower = 0, finite = TRUE)

  # FIXME: data.table NSE warnings
  term <- term_list <- degree <- m <- m_rel <- ..intercept <- NULL

  xcomp <- data.table::copy(object$m)
  intercept <- object$intercept
  # pred <- rowSums(xcomp) + intercept

  # longify for extraction
  xcomp[, ".id" := seq_len(.N)]
  m_long <- data.table::melt(xcomp, id.vars = ".id", value.name = "m",
                             variable.name = "term", variable.factor = FALSE)

  m_aggr <- m_long[, list(m = mean(abs(m))), by = "term"]
  m_aggr[, term_list := lapply(term, function(x) unlist(strsplit(x, ":")))]
  m_aggr[, degree := vapply(term_list, length, integer(1))]
  m_aggr[, m_rel := (m / ..intercept)]

  data.table::setorder(m_aggr, -m)
  data.table::setcolorder(m_aggr, c("degree", "term", "term_list", "m", "m_rel"))
  class(m_aggr) <- c("glex_vi", class(m_aggr))
  m_aggr[m_rel >= threshold]
}
