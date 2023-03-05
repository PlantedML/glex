# glex 0.3.1

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
