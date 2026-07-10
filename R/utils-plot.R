#' Create component label from predictors
#'
#' @keywords internal
#' @noRd
label_m <- function(predictors, mathy = TRUE) {
  preds <- paste0(sort(predictors), collapse = ", ")

  if (mathy) {
    as.expression(bquote(hat(m)[plain(.(preds))]))
  } else {
    sprintf("m(%s)", preds)
  }
}

#' Look up terms with their original values
#'
# Used to be used in `glex_explain()`, currently unused but kept in case it comes in handy.
# @noRd
# @keywords internal
# @param term (`character`) One or more terms to be looked up in `x`, e.g. `"x1"`, `c("x1", "x2:x3")`
# @param x Dataset with original observations.
# label_m_x <- function(term, x) {
#
#   vapply(term, function(m) {
#     m <- unlist(strsplit(m, split = ":"))
#     xvals <- sapply(m, function(mx) {
#       xval <- x[[mx]]
#       if (is.factor(xval)) xval <- as.character(xval)
#       if (is.numeric(xval)) xval <- format(xval, scipen = 4, justify = "none", digits = 4)
#       xval
#     })
#
#     paste0(sprintf("%s [%s]", m, xvals), collapse = "\n")
#   }, FUN.VALUE = "")
#
# }

#' Utility to get symmetric range of component
#' @keywords internal
#' @noRd
get_m_limits <- function(xdf) {
  c(-1, 1) * max(abs(xdf[["m"]]))
}

#' Utility to get predictor types from training data
#' @keywords internal
#' @noRd
get_x_types <- function(components, predictors) {
  # Create look-up table for predictors and their types
  tp <- c(
    numeric = "continuous",
    integer = "continuous",
    character = "categorical",
    factor = "categorical",
    ordered = "categorical"
  )
  x_types <- vapply(
    predictors,
    function(p) {
      cl <- class(components[["x"]][[p]])[1]
      tp[cl]
    },
    ""
  )
  checkmate::assert_subset(
    x_types,
    c("categorical", "continuous"),
    empty.ok = FALSE
  )
  x_types
}

#' Consistent diverging color scale
#' @noRd
#' @keywords internal
#' @importFrom scico scale_color_scico
diverging_palette <- function(...) {
  guide_colorbar <- ggplot2::guide_colorbar(
    barwidth = ggplot2::unit(10.2, "lines"),
    barheight = ggplot2::unit(1, "char"),
    title.position = "bottom",
    title.hjust = .5,
    title.vjust = 1
  )

  pal <- getOption("glex.palette", NULL)

  if (is.null(pal)) {
    # Default: shap/shapiq-style gradient built from the same endpoints
    # as the sign colors used in glex_explain()
    cols <- sign_colors()
    ggplot2::scale_color_gradient2(
      low = cols[["-1"]],
      mid = "#F7F7F7",
      high = cols[["1"]],
      midpoint = 0,
      guide = guide_colorbar,
      ...
    )
  } else {
    scico::scale_color_scico(
      palette = pal,
      guide = guide_colorbar,
      midpoint = 0,
      begin = 0.1,
      end = 0.9,
      ...
    )
  }
}

#' Consistent discrete color scale for categorical predictors
#' @noRd
#' @keywords internal
discrete_palette <- function(...) {
  ggplot2::scale_color_brewer(
    palette = getOption("glex.palette_discrete", "Dark2"),
    ...
  )
}

#' Colors for negative/zero/positive contributions in glex_explain()
#' Defaults follow the blue/red convention of shap/shapiq.
#' @noRd
#' @keywords internal
sign_colors <- function() {
  cols <- getOption("glex.colors_sign", c("#008BFB", "#FF0051"))
  checkmate::assert_character(cols, len = 2, any.missing = FALSE)
  c("-1" = cols[[1]], "0" = "grey50", "1" = cols[[2]])
}

#' Color for main effect lines/columns
#' @noRd
#' @keywords internal
main_effect_color <- function() {
  getOption("glex.color_line", "#194155")
}
