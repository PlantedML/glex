# Changelog

## glex 0.5.2

- **Fix newer xgboost R package compatibility**:
  - Updated tree schema column name from `Quality` to `Gain`, matching
    xgboost commit 73713de
    (`[R] rename Quality -> Gain (`[`#9938`](https://github.com/PlantedML/glex/issues/9938)`)`,
    in upstream v2.1.0)
  - Implemented dynamic `base_score` extraction to replace hardcoded 0.5
    intercept (modern xgboost auto-estimates `base_score`)
  - Fixed floating-point precision mismatch in C++ split comparators by
    casting to float, matching xgboost predictor behavior
  - Added node reindexing to ensure contiguous row ordering in tree
    matrices
  - For CRAN users, this schema change is observed in the later 3.x
    package line (for example 3.1.2.1+), which requires R \>= 4.3.0
  - These changes ensure accurate model explanations for current CRAN
    xgboost releases

## glex 0.5.1

- Fix path-dependent algorithm by computing the proper covers manually
- Allow [`glex()`](http://plantedml.com/glex/reference/glex.md) to
  accept data frames as input

## glex 0.5.0

- Optimize FastPD to be able to handle more features using bitmask
  represenation ([\#29](https://github.com/PlantedML/glex/issues/29))
- Remove old `probFuntion` parameter to
  [`glex()`](http://plantedml.com/glex/reference/glex.md) in favor of
  `weighting_method`.
- Add new progress bar when explaining many trees using
  [`glex()`](http://plantedml.com/glex/reference/glex.md)

## glex 0.4.2

- Optimize FastPD by only computing components up to `max_interaction`
  ([\#24](https://github.com/PlantedML/glex/issues/24))

## glex 0.4.1

- Added FastPD ([arXiv](https://arxiv.org/abs/2410.13448)) as default
  `probFunction` in `glex`.
- Add rug plot to `plot_*_effect[s]` functions for continuous
  predictors, defaulting to showing a rug on the bottom side
  (`rug_side = "b"`).

## glex 0.4.0

- Add support for ranger objects to
  [`glex()`](http://plantedml.com/glex/reference/glex.md)
  ([PR#17](https://github.com/PlantedML/glex/pull/17)).
- Add new optional parameter `probFunction` to
  [`glex()`](http://plantedml.com/glex/reference/glex.md) which
  specifies the probability function for weighting/marginalization of
  the leaves ([PR#17](https://github.com/PlantedML/glex/pull/17)).  
  By default, [`glex()`](http://plantedml.com/glex/reference/glex.md)
  now uses the empirical marginal probabilities to perform the
  weighting. Previously, the weighting of the leaves was done based on a
  path-dependent method.
- Add
  [`theme_glex()`](http://plantedml.com/glex/reference/theme_glex.md) as
  a default theme to all plots.  
  This is almost identical to
  \[[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)\]
  aside from increased base font size and convenience flags to toggle
  vertical and horizontal grid lines.
- Add
  [`subset_components()`](http://plantedml.com/glex/reference/subset_components.md)
  and
  [`subset_component_names()`](http://plantedml.com/glex/reference/subset_components.md)
  to make it easier to extract only components belonging to a given main
  term.
- Add pre-processed version of `Bikeshare` data from `ISLR2` to
  streamlined examples.
- Add [`plot_pdp()`](http://plantedml.com/glex/reference/plot_pdp.md), a
  version of
  [`plot_main_effect()`](http://plantedml.com/glex/reference/plot_components.md)
  with the intercept added.
- Limit `max_interaction` in `glex.xgb.Booster` to `max_depth` parameter
  of `xgboost` model. If `max_depth` is not set during model fit, the
  default value of `6` is assumed. This prevents `glex` from returning
  spurious higher-order interactions containing values numerically close
  to 0.
- Extend plot functions to multiclass classification. In most cases that
  means facetting by the target class.
- Overhaul `glex_explain` to a waterfall plot showing the SHAP
  decomposition for given predictors.
- `autoplot.glex_vi` gains a `max_interaction` argument in line with
  `glex_explain`, and now similarly aggregates terms that either fall
  below `threshold` or exceed `max_interaction`.
- Add `glex.print` for a more compact output in case of large numbers of
  terms.

## glex 0.3.0

- Added plotting functions for main, 2- and 3-degree interaction terms
- Added
  [`ggplot2::autoplot`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  S3 method for `glex` objects.
- Added `pkgdown` site
- Added Bikesharing article
- Added [`glex_vi()`](http://plantedml.com/glex/reference/glex_vi.md) to
  compute variable importance scores including interaction terms,
  including a corresponding
  [`ggplot2::autoplot`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  method.
- Added
  [`glex_explain()`](http://plantedml.com/glex/reference/glex_explain.md)
  to plot prediction components of a single observation.

## glex 0.2.0

- Convert [`glex()`](http://plantedml.com/glex/reference/glex.md) to an
  S3 generic function with methods for `xgboost` and
  `randomPlantedForest` models.
- Fix bug in `xgboost` method that could lead to wrongly computed shap
  values in certain cases.
- Added a `NEWS.md` file to track changes to the package.
