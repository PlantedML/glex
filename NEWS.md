# glex 0.5.1

* Fix path-dependent algorithm by computing the proper covers manually
* Allow `glex()` to accept data frames as input


# glex 0.5.0

* Optimize FastPD to be able to handle more features using bitmask represenation (#29)
* Remove old `probFuntion` parameter to `glex()` in favor of `weighting_method`. 
* Add new progress bar when explaining many trees using `glex()` 

# glex 0.4.2

* Optimize FastPD by only computing components up to `max_interaction` (#24)

# glex 0.4.1

* Added FastPD ([arXiv](https://arxiv.org/abs/2410.13448)) as default `probFunction` in `glex`.
* Add rug plot to `plot_*_effect[s]` functions for continuous predictors, defaulting to showing a rug on the bottom side (`rug_side = "b"`).

# glex 0.4.0

* Add support for ranger objects to `glex()` ([PR#17](https://github.com/PlantedML/glex/pull/17)).
* Add new optional parameter `probFunction` to `glex()` which specifies the probability function for weighting/marginalization of the leaves ([PR#17](https://github.com/PlantedML/glex/pull/17)).  
  By default, `glex()` now uses the empirical marginal probabilities to perform the weighting. Previously, the weighting of the leaves was done based on a path-dependent method.
* Add `theme_glex()` as a default theme to all plots.  
  This is almost identical to [`ggplot2::theme_minimal()`] aside from increased base font size
  and convenience flags to toggle vertical and horizontal grid lines.
* Add `subset_components()` and `subset_component_names()` to make it easier to extract only components belonging to a given main term.
* Add pre-processed version of `Bikeshare` data from `ISLR2` to streamlined examples.
* Add `plot_pdp()`, a version of `plot_main_effect()` with the intercept added.
* Limit `max_interaction` in `glex.xgb.Booster` to `max_depth` parameter of `xgboost` model.
  If `max_depth` is not set during model fit, the default value of `6` is assumed.
  This prevents `glex` from returning spurious higher-order interactions containing values numerically close to 0.
* Extend plot functions to multiclass classification. In most cases that means facetting by the target class.
* Overhaul `glex_explain` to a waterfall plot showing the SHAP decomposition for given predictors.
* `autoplot.glex_vi` gains a `max_interaction` argument in line with `glex_explain`, and now similarly aggregates terms that either fall below `threshold` or exceed `max_interaction`.
* Add `glex.print` for a more compact output in case of large numbers of terms.

# glex 0.3.0

* Added plotting functions for main, 2- and 3-degree interaction terms
* Added `ggplot2::autoplot` S3 method for `glex` objects.
* Added `pkgdown` site
* Added Bikesharing article
* Added `glex_vi()` to compute variable importance scores including interaction terms, including a
  corresponding `ggplot2::autoplot` method.
* Added `glex_explain()` to plot prediction components of a single observation.

# glex 0.2.0

* Convert `glex()` to an S3 generic function with methods for `xgboost` and `randomPlantedForest` models.
* Fix bug in `xgboost` method that could lead to wrongly computed shap values in certain cases.
* Added a `NEWS.md` file to track changes to the package.
