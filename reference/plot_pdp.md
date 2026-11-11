# Partial Dependence Plot

A version of
[`plot_main_effect`](https://plantedml.com/glex/reference/plot_components.md)
with the intercept term (horizontal line) added, resulting in a partial
dependence plot.

## Usage

``` r
plot_pdp(object, predictor, rug_sides = "b", ...)
```

## Arguments

- object:

  Object of class
  [`glex`](https://plantedml.com/glex/reference/glex.md).

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

[`plot_main_effect()`](https://plantedml.com/glex/reference/plot_components.md)

Other Visualization functions:
[`plot.glex()`](https://plantedml.com/glex/reference/plot_components.md),
[`plot.glex_vi()`](https://plantedml.com/glex/reference/plot.glex_vi.md),
[`plot_force()`](https://plantedml.com/glex/reference/plot_force.md),
[`plot_shap_decomposition()`](https://plantedml.com/glex/reference/plot_shap_decomposition.md),
[`plot_waterfall()`](https://plantedml.com/glex/reference/plot_waterfall.md)

## Examples

``` r
library(randomPlantedForest)

# introduce factor variables to show categorical feature handling
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)

# Fit the model, get components
set.seed(12)
rpfit <- rpf(mpg ~ cyl + wt + hp + drat + vs, data = mtcars, max_interaction = 3)
components <- glex(rpfit, mtcars)

plot_pdp(components, "wt")

plot_pdp(components, "cyl")
```
