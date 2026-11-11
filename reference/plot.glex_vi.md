# Plot glex Variable Importances

Plot glex Variable Importances

## Usage

``` r
# S3 method for class 'glex_vi'
plot(
  x,
  by_degree = FALSE,
  threshold = NULL,
  max_interaction = NULL,
  scale = "absolute",
  ...
)
```

## Arguments

- x:

  Object of class `glex_vi`, see
  [`glex_vi()`](https://plantedml.com/glex/reference/glex_vi.md).

- by_degree:

  (`logical(1): FALSE`) Optionally sum values by degree of interaction,
  resulting in one contribution score for all main effects, all
  second-order interactions, etc.

- threshold:

  (`numeric(1): NULL`) Terms with an importance score at or below this
  are aggregated under a `"Remaining terms"` label. Refers to the chosen
  `scale` (and the `measure` of the
  [`glex_vi()`](https://plantedml.com/glex/reference/glex_vi.md)
  object). The default `NULL` uses 2.5% of the largest score (per class
  for multiclass objects), so negligible terms do not crowd the plot,
  and `0` with `by_degree = TRUE`. Set `0` to show all terms.

- max_interaction:

  (`integer(1): NULL`) Optionally filter plot to show terms up to the
  specified degree of interaction. Similar to `threshold`, all other
  terms will be aggregated under a `"Remaining terms"` label.

- scale:

  (`"absolute"`) Plot `m` (default) or `m_rel` (`"relative"`) of
  [`glex_vi()`](https://plantedml.com/glex/reference/glex_vi.md): for
  its default `measure = "mean_abs"`, the average absolute contributions
  or the same value scaled by the average prediction; for `"variance"`,
  the mean square or its share of the variance of the prediction; for
  `"share"`, the pruning's share either way.

- ...:

  (Unused)

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## See also

[glex_vi](https://plantedml.com/glex/reference/glex_vi.md)

Other Visualization functions:
[`plot.glex()`](https://plantedml.com/glex/reference/plot_components.md),
[`plot_force()`](https://plantedml.com/glex/reference/plot_force.md),
[`plot_pdp()`](https://plantedml.com/glex/reference/plot_pdp.md),
[`plot_shap_decomposition()`](https://plantedml.com/glex/reference/plot_shap_decomposition.md),
[`plot_waterfall()`](https://plantedml.com/glex/reference/plot_waterfall.md)

## Examples

``` r
library(xgboost)
x <- as.matrix(mtcars[, -1])
xg <- xgboost(x, mtcars$mpg, max_depth = 3, nrounds = 10, nthreads = 1)
vi <- glex_vi(glex(xg, x))

plot(vi, threshold = 0.05)

plot(vi, by_degree = TRUE)
```
