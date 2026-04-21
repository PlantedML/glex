# A ggplot2 theme for glex plots

This is a slight variation of
[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
with increased font size.

## Usage

``` r
theme_glex(
  base_size = 13,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22,
  grid_x = TRUE,
  grid_y = FALSE
)
```

## Arguments

- base_size:

  (`13`) Base font size, given in pts.

- base_family:

  (`""`) Base font family

- base_line_size, base_rect_size:

  (`base_size / 22`) Base size for line and rect elements

- grid_x:

  (`TRUE`) Display horizontal grid lines?

- grid_y:

  (`FALSE`) Display vertical grid lines?

## Value

A `ggplot2` theme object

## Examples

``` r
library(ggplot2)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  theme_glex()
```
