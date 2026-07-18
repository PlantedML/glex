#' Package options for glex plots
#'
#' The color choices used across all `glex` visualization functions can be
#' adjusted globally via [options()]:
#'
#' * `glex.palette` (`NULL`): Diverging palette used to color continuous
#'   interaction effects in [plot_twoway_effects()] and
#'   [plot_threeway_effects()]. The default `NULL` uses a blue/red gradient
#'   built from `glex.colors_sign`, matching the look of the Python `shap`
#'   and `shapiq` packages. Set to the name of a diverging
#'   [scico][scico::scico] palette (e.g. `"vik"`, `"roma"`) to use that
#'   instead.
#' * `glex.palette_discrete` (`"Dark2"`): Discrete palette used to color
#'   categorical predictors in interaction plots. Accepts a vector of colors
#'   (used via [ggplot2::scale_color_manual()]), the string `"okabe-ito"`
#'   (the colorblind-safe Okabe-Ito palette via [grDevices::palette.colors()]),
#'   the name of a [scico][scico::scico] palette, or the name of an
#'   [RColorBrewer][ggplot2::scale_color_brewer] palette.
#' * `glex.colors_sign` (`c("#008BFB", "#FF0051")`): Two colors for
#'   negative and positive contributions in [glex_explain()], also used as
#'   the endpoints of the default continuous gradient. The defaults
#'   follow the blue/red convention familiar from the Python `shap` and
#'   `shapiq` packages.
#' * `glex.color_line` (`"#194155"`): Color for main effect lines and
#'   columns drawn by `autoplot()` and [plot_pdp()].
#'
#' @examples
#' # Use a scico palette for continuous effects instead of the default gradient
#' options(glex.palette = "roma")
#'
#' # Restore the default shap-style gradient
#' options(glex.palette = NULL)
#'
#' # Categorical predictors: Okabe-Ito, a scico/brewer palette, or custom colors
#' options(glex.palette_discrete = "okabe-ito")
#' options(glex.palette_discrete = "batlow")
#' options(glex.palette_discrete = c("#E69F00", "#56B4E9", "#009E73"))
#'
#' @name glex_options
NULL
