#' Variable Importance for Main and Interaction Terms
#'
#' @param object Object of class `glex`.
#' @param measure (`character(1): "mean_abs"`) How a term's importance is measured, see Details:
#'   * `"mean_abs"`: its mean absolute value at `x`.
#'   * `"variance"`: its mean square at `x`, and as a share of the variance of the prediction.
#'   * `"share"`: for the `glex()` object of a pruned Random Planted Forest only: the pruning's own share of the
#'     variance of the prediction on the training rows (`object$pruned$components$share`), by which the pruning
#'     chose the terms.
#' @param ... (Unused)
#'
#' @returns A [data.table::data.table()] with columns:
#' * `degree` (`integer`): Degree of interaction of the `term`, with `1` being main effects,
#'    `2` being 2-degree interactions etc.
#' * `term` (`character`): Model term, e.g. main effect `x1` or interaction term `x1:x2`, `x1:x3:x5` etc.
#' * `class` (`factor`): For multiclass targets and `measure = "mean_abs"` only: The associated target class. Lists all
#'   classes in the target, not limited to the majority vote.
#' * `m` (`numeric`): The importance of `term` by `measure`, see Details.
#' * `m_rel` (`numeric`): `m` relative to a scale, see Details.
#'
#' The `measure` is stored as the attribute `"measure"`. Terms with `m` of zero are left out; the rows are sorted by
#' decreasing `m`.
#'
#' @export
#'
#' @details
#' **`measure = "mean_abs"`** (the default). The `m` reported is the average absolute value of `m` as reported by
#' `glex()`, aggregated by `term` (and by class for multiclass models):
#'
#' \deqn{\mathtt{m} = \frac{1}{n} \sum_{i = 1}^n |m_S(x_i)| }
#'
#' In turn, `m_rel` rescales `m` by the average prediction of the model (\eqn{m_0}, `intercept` as reported by `glex()`;
#' its class's for models with one intercept per class):
#'
#' \deqn{\mathtt{m\_rel} = \frac{\mathtt{m}}{m_0}}
#'
#' It is in units of the prediction: how far the term moves a typical prediction.
#'
#' **`measure = "variance"`.** `m` is the mean square of the term,
#'
#' \deqn{\mathtt{m} = \frac{1}{n} \sum_{i = 1}^n m_S(x_i)^2, }
#'
#' which is the mean squared change of the prediction if the term were dropped: what dropping it costs in squared
#' error. `m_rel` is `m` over the variance of the prediction at `x` (the intercept plus all terms, and `remainder` if
#' any), its share of the variance of the prediction. For the components of a purified decomposition (mean zero), `m`
#' is the variance of the term. For multiclass models, `m` and the variance of the prediction are summed over the
#' classes' scores (one row per term). This is the size by which randomPlantedForest's `prune_components()` ranks and
#' prunes the components, except that it measures it on the training rows: for the `glex()` object of a pruned
#' model, `measure = "share"` gives the pruning's own numbers. (For classification with `loss = "logit"` or
#' `"exponential"`, randomPlantedForest reports the scores centred across the classes, as the pruning measures them.)
#'
#' **`measure = "share"`.** For the `glex()` object of a pruned Random Planted Forest: `m` and `m_rel` are both the
#' pruning's share of the variance of the prediction on the training rows, as in `object$pruned$components`.
#'
#' The two measures can rank terms differently: a term that is large on a few observations and zero elsewhere ranks
#' higher by its variance than by its mean absolute value.
#'
#' @seealso [plot.glex_vi()]
#' @examplesIf requireNamespace("xgboost", quietly = TRUE)
#' set.seed(1)
#'
#' # xgboost -----
#' library(xgboost)
#' x <- as.matrix(mtcars[, -1])
#' y <- mtcars$mpg
#' xg <- xgboost(x[1:26, ], y[1:26],
#'   max_depth = 4, learning_rate = .1,
#'   nrounds = 10, verbosity = 0, nthreads = 1
#' )
#' glex_xgb <- glex(xg, x[27:32, ])
#' vi_xgb <- glex_vi(glex_xgb)
#'
#' plot(vi_xgb)
#' plot(vi_xgb, by_degree = TRUE)
#'
#' # by the share of the variance of the prediction
#' plot(glex_vi(glex_xgb, measure = "variance"), scale = "relative")
glex_vi <- function(object, measure = c("mean_abs", "variance", "share"), ...) {
  checkmate::assert_class(object, classes = "glex")
  measure <- match.arg(measure)

  term <- degree <- m <- m_rel <- NULL
  class <- NULL

  m_long <- melt_m(object$m, object$target_levels)

  multiclass <- !is.null(object$target_levels) && measure == "mean_abs"
  if (multiclass) {
    vars_by <- c("term", "class")
    vars_out <- c("degree", "term", "class", "m", "m_rel")
  } else {
    vars_by <- "term"
    vars_out <- c("degree", "term", "m", "m_rel")
  }

  # aggregate by term, add useful variables
  if (measure == "mean_abs") {
    m_aggr <- m_long[, list(m = mean(abs(m))), by = vars_by]
  } else if (measure == "variance") {
    # mean square per score column, summed over the classes
    m_aggr <- m_long[,
      list(m = mean(m^2)),
      by = c(vars_by, intersect("class", names(m_long)))
    ]
    m_aggr <- m_aggr[, list(m = sum(m)), by = vars_by]
  } else {
    if (!is_pruned_glex(object)) {
      stop(
        "`measure = \"share\"` needs the `glex()` object of a pruned Random Planted Forest; ",
        "use `measure = \"variance\"` for other models."
      )
    }
    comp <- object$pruned$components
    m_aggr <- data.table::data.table(term = unique(m_long$term))
    m_aggr[, m := comp$share[match(term, comp$term)]]
  }
  # Only return non-zero scores
  m_aggr <- m_aggr[!is.na(m_aggr[["m"]]) & m_aggr[["m"]] > 0]
  m_aggr[, degree := get_degree(term)]
  if (measure == "mean_abs") {
    m_aggr[,
      m_rel := m /
        class_intercept(object, if (multiclass) as.character(class) else NULL)
    ]
  } else if (measure == "variance") {
    m_aggr[, m_rel := (m / prediction_variance(object))]
  } else {
    m_aggr[, m_rel := m]
  }

  data.table::setorder(m_aggr, -m)
  data.table::setcolorder(m_aggr, neworder = vars_out)
  data.table::setattr(m_aggr, "measure", measure)
  class(m_aggr) <- c("glex_vi", class(m_aggr))
  m_aggr
}

#' The variance of the prediction at x (summed over the classes' scores for multiclass models)
#'
#' The prediction is the intercept plus all terms plus `remainder` (if the decomposition is constrained); the intercept
#' does not change the variance.
#' @keywords internal
#' @noRd
prediction_variance <- function(object) {
  pop_var <- function(v) mean((v - mean(v))^2)
  rem <- object$remainder
  if (is.null(object$target_levels)) {
    pred <- rowSums(object$m)
    if (is.numeric(rem)) {
      pred <- pred + rem
    }
    return(pop_var(pred))
  }
  term_class <- split_names(names(object$m), "__class:", target_index = 2)
  sum(vapply(
    object$target_levels,
    function(level) {
      pred <- rowSums(object$m[, term_class == level, with = FALSE])
      if (is.data.frame(rem) && level %in% names(rem)) {
        pred <- pred + rem[[level]]
      }
      pop_var(pred)
    },
    numeric(1)
  ))
}

#' The terms of a glex object by decreasing importance (`glex_vi()`'s `measure`; summed over classes for multiclass
#' models): a named numeric vector, for `"variance"` its share of the variance of the prediction
#' @keywords internal
#' @noRd
component_ranking <- function(object, measure) {
  vi <- glex_vi(object, measure = measure)
  value <- if (measure == "mean_abs") vi$m else vi$m_rel
  rank <- tapply(value, vi$term, sum)
  rank <- stats::setNames(as.numeric(rank), names(rank))
  rank[order(-rank)]
}
