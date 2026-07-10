#' Package options for glex plots
#'
#' The color choices used across all `glex` visualization functions can be
#' adjusted globally via [options()]:
#'
#' \describe{
#'   \item{`glex.palette`}{(`"vik"`) Name of a diverging [scico][scico::scico]
#'     palette used to color continuous interaction effects in
#'     [plot_twoway_effects()] and [plot_threeway_effects()].}
#'   \item{`glex.palette_discrete`}{(`"Dark2"`) Name of a discrete
#'     [RColorBrewer][ggplot2::scale_color_brewer] palette used to color
#'     categorical predictors in interaction plots.}
#'   \item{`glex.colors_sign`}{(`c("#008BFB", "#FF0051")`) Two colors for
#'     negative and positive contributions in [glex_explain()]. The defaults
#'     follow the blue/red convention familiar from the Python `shap` and
#'     `shapiq` packages.}
#'   \item{`glex.color_line`}{(`"#194155"`) Color for main effect lines and
#'     columns drawn by `autoplot()` and [plot_pdp()].}
#' }
#'
#' @examples
#' # Align continuous effects with a different scico palette
#' options(glex.palette = "roma")
#'
#' # Restore defaults
#' options(glex.palette = NULL)
#'
#' @name glex_options
NULL
