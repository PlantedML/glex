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
    return(xdf[mwide])
  }

  xdf$m <- object$m[[paste(sort(predictors), collapse = ":")]]
  xdf[]
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
    m_long[, class := multiclass_m_to_class(term, levels)]
    m_long[, term := multiclass_m_to_term(term)]
  }

  m_long
}

multiclass_m_to_class <- function(mn, levels) {
  ret <- gsub(pattern = "__class:", replacement = "", regmatches(mn, regexpr("__class:(.*)$", mn)))
  factor(ret, levels = levels)
}

multiclass_m_to_term <- function(mn) {
  gsub(pattern = "__class:(.*)$", replacement = "", mn)
}

reshape_m_multiclass <- function(object) {
  checkmate::assert_subset(format, choices = c("wide", "long"))
  checkmate::assert_character(object$target_levels, min.len = 2)

  mlong <- melt_m(object$m, object$target_levels)
  data.table::dcast(mlong, .id + class  ~ term, value.var = "m")
}

#' Get the degree of interaction from vector of terms
#'
#' Terms of the form `"x1", "x2", "x1:x2"` have degrees 1, 1, 2, respectively.
#' This utility function exists mainly for code deduplication and consistency.
#' It is also a lot faster than using regex-approaches, yet it still makes the strong assumption
#' of `:` _only_ occuring as an interaction delimiter.
#'
#' @noRd
#' @keywords internal
get_degree <- function(x, pattern = ":") {
  lengths(strsplit(x, split = ":", fixed = TRUE))
}
