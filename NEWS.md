# glex 0.7.0.9000 (development version

* `glex()` supports `ranger` models with factor features under every `respect.unordered.factors` option.
 Pass the training-style `data.frame` as `x`: factors are encoded as in `predict()`, including the    response-based level order of `"order"`, `"partition"` splits are evaluated as level sets, and `$x` keeps the factor columns. 
 Previously, factor columns errored and `"partition"` models failed. `weighting_method = "empirical"` errors on `"partition"` splits.

* `glex()` methods warn about arguments they do not use instead of silently ignoring them, e.g. `glex(rp, x, weighting_method = "fastpd")` for an `rpf` model, which has no weighting method, or a misspelled `weighting_methd` for `xgboost` and `ranger`.

* Multiclass terms such as `mpg__class:4` no longer count the class suffix as an interaction, so `print()` reports the correct maximum degree for multiclass `rpf` models.

* New `plot_waterfall()` and `plot_force()` show how a single prediction is built from the components of the decomposition, one bar or segment per main or interaction term, from E[f] to f(x). Small terms are aggregated via `max_terms`, `threshold` and `max_interaction`; constrained decompositions end with a remainder bar (#49).

* `glex_explain()` is renamed to `plot_shap_decomposition()`, which says what it shows: how each feature's SHAP value is assembled from the components it is part of. `glex_explain()` still works but is deprecated. The plot no longer clips labels, places values beside the bars and the SHAP value in its own bottom row, drops empty rows in facets, and keeps "Remaining terms" directly above the SHAP value.

* Multiclass `rpf` models now carry one intercept per class (fixed upstream in randomPlantedForest), and `glex_vi()`, `plot_shap_decomposition()`, `plot_waterfall()` and `plot_force()` use each class's intercept. Constraints on multiclass `rpf` decompositions are now confirmed against the model's predictions like for other models, so a constraint that only drops zero terms keeps the SHAP values, and `$remainder` is computed by glex for each class. `print()` and the plot captions now use singular forms where appropriate ("1 observation", "1 term").

# glex 0.7.0

## Breaking changes

* `$shap` is now a scalar `NA` (with a warning) when the decomposition is constrained via
  `max_interaction` or `features`: a constrained decomposition does not sum to the
  full model prediction, so SHAP values cannot be reconstructed from it without
  violating the efficiency property. Previously, misleading values were returned.
  `$m` is unaffected. Supersedes #18, closes #13.

## New features

* `glex()` supports `xgboost` models with categorical features, i.e. models fit
  on a `data.frame` with factor columns via `xgboost()` or `xgb.train()` on an
  `xgb.DMatrix` built from one. Pass the same `data.frame` as `x`; the factor
  columns are kept in `$x`, so plots treat them as categorical. Factor levels must
  match the training data in order, as for `predict()`. Trees are now read from
  the model's JSON representation instead of `xgb.model.dt.tree()`, which refuses
  such models. `weighting_method = "empirical"` does not support categorical
  splits and errors.

* New `group_components()` aggregates the terms of a decomposition so that a set of
  features is treated as one feature (#26), e.g. re-assembling a dummy-encoded factor:
  `group_components(gl, groups = list(f = c("fa", "fb", "fc")))`. The regrouping is
  exact (components still sum to the prediction, group SHAP values are sums of member
  SHAP values), and `$x` gains a reconstructed factor for dummy-encoded groups so the
  plot functions work on grouped objects. The companion `dummy_groups()` derives the
  `groups` list from the original un-encoded data, matching `model.matrix()` column
  naming by default and taking a `naming` function for other encoding schemes. The
  new vignette "Categorical features" walks through native factors, one-hot groups
  and semantic groups.

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

* `print()` on a `glex` object reports when the decomposition is constrained.

## Bug fixes

* Term names of `xgboost` and `ranger` decompositions no longer depend on the
  column order of `x`: the default `fastpd` and `path-dependent` methods named
  interaction terms in feature-index order (e.g. `wt:hp`), while the plotting
  functions and `subset_components()` look up the sorted name (`hp:wt`), so
  interaction plots failed for any model whose features are not in alphabetical
  order. Terms are now always sorted, as for `weighting_method = "empirical"`.

* `glex()` now confirms the decomposition of binary models fit with `xgboost()`
  (class `xgboost`) against the margin. Previously `predict()` silently returned
  probabilities for those, as its method ignores `outputmargin`.

* Plotting a term that is not part of the decomposition now fails with an informative
  error instead of `non-numeric argument to mathematical function`. Tree models only
  yield components for feature subsets that occur together in at least one tree, so
  e.g. `plot_twoway_effects()` on two features never split on together (common with
  one-hot encoded categoricals) has no `m` column to plot.

* `glex()` now warns when `x` contains missing values (#41): splits are evaluated
  without the model's learned missing-value direction, so the decomposition of rows
  with `NA`s is unreliable and does not sum to the model prediction. Proper missing
  value support needs more investigation; previously such input passed silently.

* `glex()` on `xgboost` models fit with early stopping now decomposes only the trees
  up to `best_iteration`, matching what `predict()` evaluates by default. Previously
  all fitted trees were decomposed, so the components did not sum to the prediction.
  Closes #42.

* For `randomPlantedForest` classification models, `glex()` now confirms the constraint
  against `predict(type = "numeric")` rather than the default `type = "prob"`. rpf
  decomposes the raw score, while `type = "prob"` applies rpf's response function
  (a clamp to `[0, 1]` for `loss = "L2"`, the inverse link for `"logit"` and
  `"exponential"`) and for binary models returns the classes in an order whose first
  column is not the one being decomposed. The components reconstruct the raw score
  exactly, so `$remainder` now measures only what the dropped terms are worth, instead of
  silently absorbing the back-transformation and the class mix-up.

## Other

* `randomPlantedForest (>= 0.3.0)` is now required (in `Suggests:`): it fixes an
  out-of-bounds read in `purify_3()` that crashed R on Windows
  (PlantedML/randomPlantedForest#61), so rpf tests and examples run on all platforms.

* `glex_explain()` now reads SHAP values from `$shap` instead of recomputing them from
  the components, so the `glex` object is the single source of truth. The SHAP
  reference bar is omitted for constrained decompositions, where it previously showed
  a value reconstructed from the constrained components, and for objects created by
  earlier versions of glex, which have no `$shap`.

* glex now declares `R (>= 4.1.0)`: the tests use the native pipe and lambda
  shorthand, which the previous `R (>= 3.0)` did not reflect.

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
