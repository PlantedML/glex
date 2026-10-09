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
  # Groups without a dummy encoding get an all-NA column from group_components()
  no_values <- predictors[vapply(
    predictors,
    function(p) all(is.na(components[["x"]][[p]])),
    logical(1)
  )]
  if (length(no_values) > 0) {
    stop(sprintf(
      "No values in `x` for %s: a feature grouped by `group_components()` without a dummy encoding has no single x value to plot against.",
      paste(sprintf("\"%s\"", no_values), collapse = ", ")
    ))
  }
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
#'
#' The glex.palette_discrete option accepts a vector of colors,
#' "okabe-ito", a scico palette name, or a brewer palette name (default).
#' @noRd
#' @keywords internal
discrete_palette <- function(...) {
  pal <- getOption("glex.palette_discrete", "Dark2")

  if (length(pal) > 1) {
    return(ggplot2::scale_color_manual(values = pal, ...))
  }

  checkmate::assert_string(pal)

  if (identical(tolower(pal), "okabe-ito")) {
    cols <- unname(grDevices::palette.colors(palette = "Okabe-Ito"))
    return(ggplot2::scale_color_manual(values = cols, ...))
  }

  if (pal %in% scico::scico_palette_names()) {
    return(scico::scale_color_scico_d(palette = pal, ...))
  }

  ggplot2::scale_color_brewer(palette = pal, ...)
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

#' Fill alpha by interaction degree: main effects opaque, aggregates at 0.5
#' @noRd
#' @keywords internal
degree_alpha <- function(degree) {
  ifelse(is.na(degree), 0.5, 1 / sqrt(degree))
}

#' Signed contribution label, e.g. "+0.21"
#' @noRd
#' @keywords internal
format_contribution <- function(m) {
  sprintf("%+.3g", m)
}

#' X expansion leaving room for value labels placed outside bar ends
#'
#' Panels get narrower with more facet columns (laid out as by `facet_wrap()`),
#' so labels need proportionally more room.
#' @param n_panels Number of facets.
#' @noRd
#' @keywords internal
label_expansion <- function(labels, n_panels = 1) {
  n_cols <- grDevices::n2mfrow(n_panels)[1]
  ggplot2::expansion(mult = min(0.8, 0.05 + 0.02 * max(nchar(labels), 0) * n_cols))
}

#' Secondary x axis marking E\[f\] and f(x)
#'
#' Close values would overlap, so they share one label.
#' @param et Result of `explain_terms()`.
#' @param xrange Width of the plotted x range.
#' @noRd
#' @keywords internal
reference_axis <- function(et, xrange) {
  fmt <- function(v) format(v, digits = 3)
  if (abs(et$prediction - et$intercept) < 0.1 * xrange) {
    return(ggplot2::dup_axis(
      name = NULL,
      breaks = (et$intercept + et$prediction) / 2,
      labels = sprintf("E[f] = %s, f(x) = %s", fmt(et$intercept), fmt(et$prediction))
    ))
  }
  ggplot2::dup_axis(
    name = NULL,
    breaks = c(et$intercept, et$prediction),
    labels = c(sprintf("E[f] = %s", fmt(et$intercept)), sprintf("f(x) = %s", fmt(et$prediction)))
  )
}

#' @noRd
#' @keywords internal
prediction_title <- function(id, et, class) {
  sprintf(
    "Prediction for observation %d%s: f(x) = %s",
    id,
    if (is.null(class)) "" else sprintf(" (class %s)", class),
    format(et$prediction, digits = 3)
  )
}

#' @noRd
#' @keywords internal
terms_subtitle <- function(et) {
  sprintf(
    "Starting from E[f] = %s; %d terms shown%s",
    format(et$intercept, digits = 3),
    sum(et$terms$type == "term"),
    if (et$n_other > 0) sprintf(", %d aggregated", et$n_other) else ""
  )
}
