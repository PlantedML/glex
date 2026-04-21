# Partial Dependence Plot

A version of
[`plot_main_effect`](http://plantedml.com/glex/reference/plot_components.md)
with the intercept term (horizontal line) added, resulting in a partial
dependence plot.

## Usage

``` r
plot_pdp(object, predictor, rug_sides = "b", ...)
```

## Arguments

- object:

  Object of class [`glex`](http://plantedml.com/glex/reference/glex.md).

- predictor:

  `(character(1))` predictor names, e.g. `"x1"` to plot main effect of
  `x1`.

- rug_sides:

  `(character(1): "b")` Sides to plot rug (see
  [`ggplot2::geom_rug()`](https://ggplot2.tidyverse.org/reference/geom_rug.html))
  plot on for continuous predictors.. Default is `"b"` for both sides.
  Set to `"none"` to disable rug plot.

- ...:

  Used for future expansion.

## Value

A `ggplot2` object.

## See also

[`plot_main_effect()`](http://plantedml.com/glex/reference/plot_components.md)

Other Visualization functions:
[`autoplot.glex()`](http://plantedml.com/glex/reference/plot_components.md),
[`autoplot.glex_vi()`](http://plantedml.com/glex/reference/autoplot.glex_vi.md),
[`glex_explain()`](http://plantedml.com/glex/reference/glex_explain.md)

## Examples

``` r
if (requireNamespace("randomPlantedForest", quietly = TRUE)) {
library(randomPlantedForest)

# introduce factor variables to show categorical feature handling
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)

# Fit forest, get components
set.seed(12)
rpfit <- rpf(mpg ~ cyl + wt + hp + drat + vs, data = mtcars, ntrees = 25, max_interaction = 3)
components <- glex(rpfit, mtcars)

plot_pdp(components, "wt")
plot_pdp(components, "cyl")
}
```
