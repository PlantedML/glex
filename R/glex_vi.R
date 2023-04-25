#' Variable Importance for Main and Interaction Terms
#'
#' @param object Object of class `glex`.
#' @param ... (Unused)
#'
#' @return A [`data.table`] with columns:
#' * `degree` (`integer`): Degree of interaction of the `term`, with `1` being main effects,
#'    `2` being 2-degree interactions etc.
#' * `term` (`character`): Model term, e.g. main effect `x1` or interaction term `x1:x2`, `x1:x3:x5` etc.
#' * `class` (`factor`): For multiclass targets only: The associated target class. Lists all classes in the
#'   target, not limited to the majority vote.
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
#' @seealso [autoplot.glex_vi]
#' @examples
#' set.seed(1)
#' # Random Planted Forest -----
#' if (requireNamespace("randomPlantedForest", quietly = TRUE)) {
#' library(randomPlantedForest)
#'
#' rp <- rpf(mpg ~ ., data = mtcars[1:26, ], max_interaction = 3)
#'
#' glex_rpf <- glex(rp, mtcars[27:32, ])
#'
#' # All terms
#' vi_rpf <- glex_vi(glex_rpf)
#'
#' library(ggplot2)
#' # Filter to contributions greater 0.05 on the scale of the target
#' autoplot(vi_rpf, threshold = 0.05)
#' # Summarize by degree of interaction
#' autoplot(vi_rpf, by_degree = TRUE)
#' # Filter by relative contributions greater 0.1%
#' autoplot(vi_rpf, scale = "relative", threshold = 0.001)
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
#' }
glex_vi <- function(object, ...) {
  checkmate::assert_class(object, classes = "glex")

  # FIXME: data.table NSE warnings
  term <- degree <- m <- m_rel <- NULL

  m_long <- melt_m(object$m, object$target_levels)

  if (!is.null(object$target_levels)) {
    vars_by <- c("term", "class")
    vars_out <- c("degree", "term", "class", "m", "m_rel")
  } else {
    vars_by <- "term"
    vars_out <- c("degree", "term", "m", "m_rel")
  }

  # aggregate by term, add useful variables
  m_aggr <- m_long[, list(m = mean(abs(m))), by = vars_by]
  # Only return non-zero scores
  m_aggr <- m_aggr[m_aggr[["m"]] > 0]
  m_aggr[, degree := get_degree(term)]
  m_aggr[, m_rel := (m / object[["intercept"]])]

  data.table::setorder(m_aggr, -m)
  data.table::setcolorder(m_aggr, neworder = vars_out)
  class(m_aggr) <- c("glex_vi", class(m_aggr))
  m_aggr
}
