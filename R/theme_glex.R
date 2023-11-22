#' A ggplot2 theme for glex plots
#'
#' This is a slight variation of [ggplot2::theme_minimal()] with increased font size.
#'
#' @param base_size (`13`) Base font size, given in pts.
#' @param base_family (`""`) Base font family
#' @param base_line_size,base_rect_size (`base_size / 22`) Base size for line and rect elements
#' @param grid_x (`TRUE`) Display horizontal grid lines?
#' @param grid_y (`FALSE`) Display vertical grid lines?
#'
#' @return A `ggplot2` theme object
#' @export
#' @import ggplot2
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_glex()
theme_glex <- function(base_size = 13, base_family = "", base_line_size = base_size / 22,
         base_rect_size = base_size / 22, grid_x = TRUE, grid_y = FALSE) {
  theme_res <- theme_bw(
    base_size = base_size, base_family = base_family,
    base_line_size = base_line_size, base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.ticks = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.position = "bottom",
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor.y = element_line(linewidth = rel(0.25)),
      strip.background = element_blank(),
      plot.background = element_blank(),
      complete = TRUE
    )

  if (!grid_y) {
    theme_res <- theme_res %+replace% theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
    )
  }

  if (!grid_x) {
    theme_res <- theme_res %+replace% theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
    )
  }

  theme_res
}
