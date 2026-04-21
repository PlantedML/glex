# Plot glex Variable Importances

Plot glex Variable Importances

## Usage

``` r
# S3 method for class 'glex_vi'
autoplot(
  object,
  by_degree = FALSE,
  threshold = 0,
  max_interaction = NULL,
  scale = "absolute",
  ...
)
```

## Arguments

- object:

  Object of class `glex_vi`, see
  [`glex_vi()`](http://plantedml.com/glex/reference/glex_vi.md).

- by_degree:

  (`logical(1): FALSE`) Optionally sum values by degree of interaction,
  resulting in one contribution score for all main effects, all
  second-order interactions, etc.

- threshold:

  (`numeric(1)`: 0) Optional threshold to filter output to include only
  importance scores greater than this value. Refers to the chosen
  `scale`.

- max_interaction:

  (`integer(1): NULL`) Optionally filter plot to show terms up to the
  specified degree of interaction. Similar to `threshold`, all other
  terms will be aggregated under a `"Remaining terms"` label.

- scale:

  (`"absolute"`) Plot average absolute contributions (default) or the
  same value but scaled by the average prediction (`"relative"`).

- ...:

  (Unused)

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## See also

[glex_vi](http://plantedml.com/glex/reference/glex_vi.md)

Other Visualization functions:
[`autoplot.glex()`](http://plantedml.com/glex/reference/plot_components.md),
[`glex_explain()`](http://plantedml.com/glex/reference/glex_explain.md),
[`plot_pdp()`](http://plantedml.com/glex/reference/plot_pdp.md)
