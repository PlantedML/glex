# glex 0.6.0.9000 (development version)

* `$shap` is now a scalar `NA` (with a warning) when the decomposition is constrained via
  `max_interaction` or `features`: a constrained decomposition does not sum to the
  full model prediction, so SHAP values cannot be reconstructed from it without
  violating the efficiency property. Previously, misleading values were returned.
  `$m` is unaffected. Supersedes #18, closes #13.
* `glex()` objects gain a `$constrained` field naming the arguments that constrained
  the decomposition (`character(0)` if complete), so `length(x$constrained) > 0` tells
  you whether `$shap` is usable.
* A requested constraint only invalidates `$shap` if it actually drops something: a
  model can contain a high-order term whose value is zero, in which case dropping it
  leaves the decomposition (and the SHAP values) unchanged. `glex()` confirms the
  constraint against the model's own predictions and, if the dropped terms were inert,
  keeps `$shap` and emits a message instead of a warning.
* `glex()` on `randomPlantedForest` models now returns `$shap` as well, computed from
  the components like for the other model classes (for multiclass models, `$shap`
  columns are class-specific like those of `$m`). Previously the field was absent.
  Constraining the decomposition post-hoc via `max_interaction` or `features` is now
  detected for `rpf` models too, where it previously passed silently.
* `glex()` objects gain a `$remainder` field: what the constraint's dropped terms are
  collectively worth, per observation, on the scale of `$m`. It is present exactly when
  the decomposition is constrained, so `intercept + rowSums(m) + remainder` reconstructs
  the model prediction whether or not a constraint was applied. `randomPlantedForest`
  objects already carried a `$remainder` computed by `predict_components()`, but the
  other model classes did not; the two are now one field with one definition, computed
  in one place. Closes #11, supersedes #25.
  Unlike #25, this covers classification and other non-identity links: the remainder is
  taken on the scale the model is decomposed on, so for `xgboost` it is on the link scale
  (`plogis(intercept + rowSums(m) + remainder)` recovers a `binary:logistic` probability),
  while `ranger` probability forests and `randomPlantedForest` are decomposed on the
  response scale directly.
* For `randomPlantedForest` classification models, `glex()` now confirms the constraint
  against `predict(type = "numeric")` rather than the default `type = "prob"`. rpf
  decomposes the raw score, while `type = "prob"` applies rpf's response function --
  a clamp to `[0, 1]` for `loss = "L2"`, the inverse link for `"logit"` and
  `"exponential"` -- and for binary models returns the classes in an order whose first
  column is not the one being decomposed. The components reconstruct the raw score
  exactly, so `$remainder` now measures only what the dropped terms are worth, instead of
  silently absorbing the back-transformation and the class mix-up.
* `glex_explain()` now reads SHAP values from `$shap` instead of recomputing them from
  the components, so the `glex` object is the single source of truth. The SHAP
  reference bar is omitted for constrained decompositions, where it previously showed
  a value reconstructed from the constrained components, and for objects created by
  earlier versions of glex, which have no `$shap`.
* `print()` on a `glex` object reports when the decomposition is constrained.
* Tests that fit `randomPlantedForest` models remain skipped on Windows: an
  out-of-bounds read in randomPlantedForest's purification crashes R there. Fixed
  upstream in randomPlantedForest 0.3.0; the skips are lifted separately, once glex
  requires that version.

# glex 0.6.0

* Extended compatibility with `xgboost`, now requiring `xgboost (>= 3.0.0)` in `Suggests:`
  - Updated tests and examples for the new API

* Plot colors are now configurable via `options()` and documented in `?glex_options`:
  `glex.palette` (diverging palette for continuous interaction effects; `NULL` for the
  default shap-style gradient, or the name of a scico palette),
  `glex.palette_discrete` (palette for categorical predictors: a color vector,
  `"okabe-ito"`, a scico palette name, or a brewer palette name),
  `glex.colors_sign` (negative/positive colors in `glex_explain()` and gradient endpoints), and
  `glex.color_line` (main effect line/column color).
* Default colors updated to follow the blue/red convention of the Python `shap`/`shapiq`
  packages: continuous interaction effects use a `#008BFB` → white → `#FF0051` gradient
  (previously the cyclic scico palette `"vikO"`), and `glex_explain()` uses the same
  blue/red for negative/positive contributions.

# glex 0.5.2

* **Fix newer xgboost R package compatibility**:
  - Updated tree schema column name from `Quality` to `Gain`, matching xgboost commit 73713de (`[R] rename Quality -> Gain (#9938)`, in upstream v2.1.0)
  - Implemented dynamic `base_score` extraction to replace hardcoded 0.5 intercept (modern xgboost auto-estimates `base_score`)
  - Fixed floating-point precision mismatch in C++ split comparators by casting to float, matching xgboost predictor behavior
  - Added node reindexing to ensure contiguous row ordering in tree matrices
  - For CRAN users, this schema change is observed in the later 3.x package line (for example 3.1.2.1+), which requires R >= 4.3.0
  - These changes ensure accurate model explanations for current CRAN xgboost releases


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
