# Plot how a feature's SHAP value is assembled from components

For one observation, shows the components involving each selected
feature, starting at the intercept E\[f\]. Interaction terms are split
evenly among their features, so each feature's bars sum to its SHAP
value, shown in the bottom row. To see the whole prediction instead, use
[`plot_waterfall()`](https://plantedml.com/glex/reference/plot_waterfall.md)
or [`plot_force()`](https://plantedml.com/glex/reference/plot_force.md).

## Usage

``` r
plot_shap_decomposition(
  object,
  id,
  threshold = NULL,
  max_interaction = NULL,
  predictors = NULL,
  class = NULL,
  barheight = 0.5
)

glex_explain(...)
```

## Arguments

- object:

  Object of class [`glex`](https://plantedml.com/glex/reference/glex.md)
  containing prediction components and data to be explained.

- id:

  (`integer(1)`) Row ID of the observation to be explained in
  `object$x`.

- threshold:

  (`numeric(1): NULL`) Terms whose contribution is at or below this are
  aggregated under a `"Remaining terms"` label. The default `NULL` uses
  2.5% of the largest absolute contribution in each panel; set `0` to
  show all terms.

- max_interaction:

  (`integer(1): NULL`) Terms of a higher degree are aggregated under the
  `"Remaining terms"` label.

- predictors:

  (`character: NULL`) Vector of column names in `$x` to restrict the
  plot to.

- class:

  (`character: NULL`) For multiclass targets, the target classes to
  show.

- barheight:

  (`numeric(1): 0.5`) Relative height of the bars.

- ...:

  Arguments passed to `plot_shap_decomposition()`.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Details

The plot can get busy; use `predictors`, `max_interaction`, or
`threshold` to restrict it.

## See also

Other Visualization functions:
[`plot.glex()`](https://plantedml.com/glex/reference/plot_components.md),
[`plot.glex_vi()`](https://plantedml.com/glex/reference/plot.glex_vi.md),
[`plot_force()`](https://plantedml.com/glex/reference/plot_force.md),
[`plot_pdp()`](https://plantedml.com/glex/reference/plot_pdp.md),
[`plot_waterfall()`](https://plantedml.com/glex/reference/plot_waterfall.md)

## Examples

``` r
set.seed(1)
library(randomPlantedForest)
rp <- rpf(mpg ~ ., data = mtcars[1:26, ], max_interaction = 2)
glex_rpf <- glex(rp, mtcars[27:32, ])

plot_shap_decomposition(glex_rpf, id = 3, predictors = "hp", threshold = 0.01)
```
