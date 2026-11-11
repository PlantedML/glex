# Plot Prediction Components

Plotting the main effects among the prediction components is effectively
identical to a partial dependence plot, centered to 0.

## Usage

``` r
# S3 method for class 'glex'
plot(
  x,
  predictors = NULL,
  ...,
  measure = NULL,
  max_components = NULL,
  ncol = NULL,
  resolution = 100L
)

plot_main_effect(object, predictor, rug_sides = "b", ...)

plot_threeway_effects(
  object,
  predictors,
  rug_sides = "b",
  resolution = 100L,
  ...
)

plot_twoway_effects(
  object,
  predictors,
  rug_sides = "b",
  resolution = 100L,
  ...
)
```

## Arguments

- x:

  Object of class
  [`glex`](https://plantedml.com/glex/reference/glex.md).

- ...:

  Used for future expansion.

- measure:

  (`character(1) | NULL: NULL`) Without `predictors`: the importance of
  each component, given in its title and ordering the plots, computed
  from the components at `x` (see
  [`glex_vi()`](https://plantedml.com/glex/reference/glex_vi.md)):

  - `"variance"`: its mean square as a share of the variance of the
    prediction at `x`, in percent;

  - `"mean_abs"`: its mean absolute value, in units of the prediction
    (`y` for regression).

  `NULL` uses `"variance"` for a Random Planted Forest from
  [`rpf()`](https://plantedml.com/randomPlantedForest/reference/rpf.html)
  of randomPlantedForest 0.6 or later (class `rpf_pruned`) and
  `"mean_abs"` otherwise.

- max_components:

  (`integer(1) | NULL: NULL`) Without `predictors`: the most components
  to draw, the most important first. `NULL` draws all components of a
  model of class `rpf_pruned` and at most 12 otherwise.

- ncol:

  (`integer(1) | NULL: NULL`) Without `predictors`: the number of
  columns of main effects and pairs. A component with several panels
  spans a row of its own. `NULL` uses up to 4.

- resolution:

  (`integer(1): 100`) Models of class `rpf_pruned` only: the most cells
  of a heat map along each predictor, see Details.

- object:

  Object of class
  [`glex`](https://plantedml.com/glex/reference/glex.md).

- predictor, predictors:

  `(character)` vector of predictor names, e.g. `"x1"` to plot main
  effect of `x1`, and `c("x1", "x2")` to plot the interaction term
  `x1:x2`.

- rug_sides:

  `(character(1): "b")` Sides to plot rug (see
  [`ggplot2::geom_rug()`](https://ggplot2.tidyverse.org/reference/geom_rug.html))
  plot on for continuous predictors.. Default is `"b"` for both sides.
  Set to `"none"` to disable rug plot.

## Value

A `ggplot2` object.

## Details

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) without
`predictors` draws the model's components in one figure (with the
patchwork package), the most important first by `measure`, each titled
by its name and importance, e.g. `x2:x3 (24%)`: main effects and two-way
interactions for any model, and for a model of class `rpf_pruned` (what
[`rpf()`](https://plantedml.com/randomPlantedForest/reference/rpf.html)
returns from randomPlantedForest 0.6 on) all of its components,
including those of three or more predictors.

**Models of class `rpf_pruned`.** Such a model has few components, each
stored in the form it predicts with. The plots show each component as it
enters the prediction:

- a main effect as a step curve through its values at `x`;

- a two-way interaction as a heat map, evaluated on a grid of the values
  of its predictors in `x`: at most `resolution` cells along each
  predictor, the distinct values if there are at most `resolution`,
  otherwise the values at the quantiles `(i - 0.5) / resolution`, each
  cell reaching halfway to its neighbours (a factor: one cell per
  level);

- a component of three or more predictors as heat maps over two of its
  predictors, one per cell of the others (the cells the model stores for
  it), each labelled by its cell, on one colour scale. The heat maps are
  exact in the other predictors: the component is constant on each cell.
  `plot_threeway_effects()` draws them for three predictors,
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for any
  number; the order of `predictors` does not matter. (A model from an
  earlier randomPlantedForest that does not store the cells gets one
  heat map, the other predictors at their values in the first row of
  `x`, with a message.)

For multiclass models every plot has one panel (or set of heat maps) per
class. The grids need the model, which the glex object keeps: a model
saved to a file and restored without
[`randomPlantedForest::rpf_unmarshal()`](https://plantedml.com/randomPlantedForest/reference/rpf_marshal.html)
can't be evaluated.

## See also

[`plot_pdp()`](https://plantedml.com/glex/reference/plot_pdp.md)

Other Visualization functions:
[`plot.glex_vi()`](https://plantedml.com/glex/reference/plot.glex_vi.md),
[`plot_force()`](https://plantedml.com/glex/reference/plot_force.md),
[`plot_pdp()`](https://plantedml.com/glex/reference/plot_pdp.md),
[`plot_shap_decomposition()`](https://plantedml.com/glex/reference/plot_shap_decomposition.md),
[`plot_waterfall()`](https://plantedml.com/glex/reference/plot_waterfall.md)

## Examples

``` r
library(randomPlantedForest)

# Every 8th hour of the bike sharing data (see ?bike)
bike_8 <- bike[seq(1, nrow(bike), by = 8), ]

# Fit the model, get components
set.seed(12)
rpfit <- rpf(bikers ~ hr + temp + workingday + season, data = bike_8, max_interaction = 3)
components <- glex(rpfit, bike_8)

# Main effects ----
plot_main_effect(components, "temp")

plot_main_effect(components, "season")


# plot() picks the plot by the number of predictors ----
plot(components, c("hr", "temp"))


# and without predictors draws the whole model ----
plot(components)

# plot_threeway_effects(components, c("hr", "temp", "workingday"))
library(randomPlantedForest)

# 2-degree interaction effects ----
# 2d continuous
plot_twoway_effects(components, c("hr", "temp"))


# continuous + categorical
plot_twoway_effects(components, c("hr", "workingday"))
```
