#' Assemble original data and corresponding component
#' @keywords internal
#' @noRd
#' @inheritParams plot_twoway_effects
assemble_components <- function(object, predictors) {
  xdf <- object$x[, predictors, with = FALSE]
  xdf[, ".id" := .I]
  setkey(xdf, ".id")

  if (!is.null(object$target_levels)) {
    mwide <- reshape_m_multiclass(object)
    mwide <- mwide[, c(".id", "class", paste(sort(predictors), collapse = ":")), with = FALSE]
    setnames(mwide, c(".id", "class", "m"))
    xdf <- xdf[mwide, on = ".id"]
  } else {
    xdf$m <- object$m[[paste(sort(predictors), collapse = ":")]]
  }

  xdf[, ".id" := NULL]
  unique(xdf)
}

#' @noRd
#' @keywords internal
melt_m <- function(m, levels = NULL) {
  # data.table NSE warning
  term <- NULL
  # We need an id.var for melt to avoid a warning, but don't want to modify m permanently
  m[, ".id" := .I]
  m_long <- data.table::melt(m, id.vars = ".id", value.name = "m",
                             variable.name = "term", variable.factor = FALSE)
  # clean up that temporary id column again while modifying by reference
  m[, ".id" := NULL]

  if (!is.null(levels)) {
    m_long[, class := split_names(term, split_string = "__class:", target_index = 2)]
    m_long[, class := factor(class, levels = levels)]
    m_long[, term := split_names(term, split_string = "__class:", target_index = 1)]
  }

  m_long
}

reshape_m_multiclass <- function(object) {
  checkmate::assert_character(object$target_levels, min.len = 2)

  mlong <- melt_m(object$m, object$target_levels)
  data.table::dcast(mlong, .id + class  ~ term, value.var = "m")
}

#' Helper to split multiclass terms of format <term>__class:<class>
#'
#' Should be faster / more consistent than using regex.
#'
#' @param mn Character vector of terms.
#' @param split_string String to split at, currently "__class:" is used as default.
#' @param target_index Index of split vector to return. Either 1 or 2 (default)
#' @keywords internal
#' @noRd
split_names <- function(mn, split_string = "__class:", target_index = 2) {
  vapply(mn, function(x) {
    unlist(strsplit(x, split = split_string, fixed = TRUE))[target_index]
  }, character(1), USE.NAMES = FALSE)
}

