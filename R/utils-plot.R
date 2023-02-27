#' Assemble original data and corresponding component
#' @keywords internal
#' @noRd
#' @inheritParams plot_twoway_effects
assemble_components <- function(components, predictors) {
  xtemp <- data.table::copy(components)
  xdf <- xtemp$x[, predictors, with = FALSE]
  xdf$m <- xtemp$m[[paste(sort(predictors), collapse = ":")]]
  xdf[]
}

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
#' Used in `glex_explain()`.
#' @noRd
#' @keywords internal
#' @param term (`character`) One or more terms to be looked up in `x`, e.g. `"x1"`, `c("x1", "x2:x3")`
#' @param x Dataset with original observations.
label_m_x <- function(term, x) {

  vapply(term, function(m) {
    m <- unlist(strsplit(m, split = ":"))
    xvals <- lapply(m, function(mx) {
      xval <- x[[mx]]
      if (is.factor(xval)) xval <- as.character(xval)
      xval
    })

    paste0(sprintf("%s [%s]", m, xvals), collapse = "\n")
  }, FUN.VALUE = "")

}

#' Utility to get symmetric range of component
#' @keywords internal
#' @noRd
get_m_limits <- function(xdf) {
  c(-1,1) * max(abs(xdf[["m"]]))
}

#' Utility to get predictor types from training data
#' @keywords internal
#' @noRd
get_x_types <- function(components, predictors) {
  # Create look-up table for predictors and their types
  tp <- c(numeric = "continuous", integer = "continuous", character = "categorical", factor = "categorical")
  x_types <- vapply(predictors, function(p) {
    cl <- class(components[["x"]][[p]])[1]
    tp[cl]
  }, "")
  checkmate::assert_subset(x_types, c("categorical", "continuous"), empty.ok = FALSE)
  x_types
}

#' Consistent diverging color scale
#' @noRd
#' @keywords internal
#' @importFrom scico scale_color_scico
diverging_palette <- function(...) {

  # guide_colorbar <- ggplot2::guide_colorbar(
  #   barwidth = ggplot2::unit(15, "char"),
  #   barheight = ggplot2::unit(1, "char"),
  #   title.position = "right",
  #   title.hjust = .5
  # )
  #
  guide_colorbar <- ggplot2::guide_colorbar(
    barwidth = ggplot2::unit(10.2, "lines"),
    barheight = ggplot2::unit(1, "char"),
    title.position = "bottom",
    title.hjust = .5, title.vjust = 1
  )

  if (!requireNamespace("scico", quietly = TRUE)) {
    ggplot2::scale_color_distiller(
      palette = "PRGn", type = "div",
      guide = guide_colorbar,
      ...
    )
  } else {
    scico::scale_color_scico(
      palette = "vikO",
      guide = guide_colorbar,
      ...
    )
  }

}
