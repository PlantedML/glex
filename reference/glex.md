# Global explanations for tree-based models.

Global explanations for tree-based models by decomposing regression or
classification functions into the sum of main components and interaction
components of arbitrary order. Calculates SHAP values and q-interaction
SHAP for all values of q for tree-based models such as xgboost.

## Usage

``` r
glex(object, x, max_interaction = NULL, features = NULL, ...)

# Default S3 method
glex(object, ...)

# S3 method for class 'rpf'
glex(object, x, max_interaction = NULL, features = NULL, ...)

# S3 method for class 'rpf_pruned'
glex(object, x, max_interaction = NULL, features = NULL, ...)

# S3 method for class 'xgb.Booster'
glex(
  object,
  x,
  max_interaction = NULL,
  features = NULL,
  max_background_sample_size = NULL,
  weighting_method = "fastpd",
  ...
)

# S3 method for class 'ranger'
glex(
  object,
  x,
  max_interaction = NULL,
  features = NULL,
  max_background_sample_size = NULL,
  weighting_method = "fastpd",
  ...
)
```

## Arguments

- object:

  Model to be explained, either of class `xgb.Booster` or `rpf`.

- x:

  Data to be explained. For
  [`xgboost`](https://rdrr.io/pkg/xgboost/man/xgb.train.html) models
  with categorical features (fit on a `data.frame` with factor columns),
  a `data.frame` whose factor columns have the same levels in the same
  order as the training data: the model stores category codes, not
  levels, so a mismatch cannot be detected beyond a missing level (this
  is the same contract as
  [`predict()`](https://rdrr.io/r/stats/predict.html)). For
  [`ranger`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  models, a `data.frame` with the same factor columns as the training
  data, encoded as [`predict()`](https://rdrr.io/r/stats/predict.html)
  does under any `respect.unordered.factors` option. Otherwise a numeric
  `matrix` or `data.frame`.

- max_interaction:

  (`integer(1): NULL`)  
  Maximum interaction size to consider. Defaults to using all possible
  interactions available in the model.  
  For [`xgboost`](https://rdrr.io/pkg/xgboost/man/xgb.train.html), this
  defaults to the `max_depth` parameter of the model fit.  
  If not set in `xgboost`, the default value of `6` is assumed.

- features:

  Vector of column names in `x` to calculate components for. Default is
  `NULL`, i.e. all features are used.

- ...:

  Further arguments passed to methods. Arguments a method does not use
  are disregarded with a warning.

- max_background_sample_size:

  The maximum number of background samples used for the FastPD
  algorithm, only used when `weighting_method = "fastpd"`. Defaults to
  `nrow(x)`.

- weighting_method:

  Use either "path-dependent", "fastpd" (default), or "empirical". See
  References for details.

## Value

Decomposition of the regression or classification function. A `list`
with elements:

- `shap`: SHAP values, derived from the functional decomposition as
  \\\phi_j = \sum\_{S \ni j} m_S / \|S\|\\. This reconstruction is only
  valid if the decomposition is complete: if it is constrained (see
  `constrained`), the components no longer sum to the full model
  prediction and the SHAP efficiency property cannot hold, so `shap` is
  a scalar `NA` (with a warning) while `m` remains valid. For multiclass
  models, columns are class-specific like those of `m`.

- `m`: Functional decomposition into all main and interaction components
  in the model, up to the degree specified by `max_interaction`. The
  variable names correspond to the original variable names, with `:`
  separating interaction terms as one would specify in a
  [`formula`](https://rdrr.io/r/stats/formula.html) interface.

- `intercept`: Intercept term, the expected value of the prediction. For
  multiclass models a named vector with one value per class.

- `constrained`: Character vector naming the arguments that constrained
  the decomposition (`"max_interaction"`, `"features"`), or
  `character(0)` if it is complete. Use `length(x$constrained) > 0` to
  check whether `shap` is valid. A constraint that only drops terms
  whose value is zero leaves the decomposition unchanged; `glex()`
  confirms this against the model's predictions, reports it with a
  message, and treats the result as complete.

- `remainder`: What the dropped terms are collectively worth, per
  observation: `prediction - (intercept + rowSums(m))`. Present exactly
  when the decomposition is constrained, and absent otherwise, so
  `intercept + rowSums(m) + remainder` reconstructs the prediction in
  either case. For multiclass `randomPlantedForest` models it is
  class-wise, mirroring `m`.

  Like `m` and `shap`, it is on the scale that the model is decomposed
  on, which for `xgboost` is the **link** scale and not the response:
  for a `binary:logistic` model the reconstruction gives the margin, and
  `plogis(intercept + rowSums(m) + remainder)` gives the predicted
  probability. `ranger` probability forests and `randomPlantedForest`
  are decomposed on the response scale, where no such
  back-transformation is needed. Adding `remainder` to a probability is
  therefore never correct for `xgboost`.

## Details

For parallel execution using `xgboost` models, register a backend, e.g.
with
[`doParallel::registerDoParallel()`](https://rdrr.io/pkg/doParallel/man/registerDoParallel.html).

**Random Planted Forests of class `rpf_pruned`.**
[`rpf()`](https://plantedml.com/randomPlantedForest/reference/rpf.html)
of randomPlantedForest (from version 0.6) returns by default a model of
class `rpf_pruned` with few components (see
[`randomPlantedForest::prune_components()`](https://plantedml.com/randomPlantedForest/reference/prune_components.html)),
each stored in the form it predicts with (scaled by its coefficient).
`glex()` of such a model returns their values at `x`, which with the
intercept sum to the model's prediction, and SHAP values derived from
them as for the other models. Components the model does not have are
absent from `m`, not zero columns. The result also carries `pruned` (see
Value), which
[`plot.glex()`](https://plantedml.com/glex/reference/plot_components.md),
[`plot_twoway_effects()`](https://plantedml.com/glex/reference/plot_components.md)
and
[`plot_threeway_effects()`](https://plantedml.com/glex/reference/plot_components.md)
use to draw the model exactly as it predicts, and which
[`glex_vi()`](https://plantedml.com/glex/reference/glex_vi.md) uses for
`measure = "share"`. A forest fitted with `prune = FALSE` (class `rpf`)
is also supported: `glex()` then decomposes the whole forest.

The different weighting methods are described in detail in Liu et al.
(2025). The default method is `"fastpd"` as it consistently estimates
the correct partial dependence function.

## Value for models of class `rpf_pruned`

The list has one more element, `pruned`, a list with

- `components`: a `data.frame` of the model's components, with columns
  `term`, `degree`, `share` (the share of the variance of the prediction
  that
  [`prune_components()`](https://plantedml.com/randomPlantedForest/reference/prune_components.html)
  measured on the training rows, before the coefficients), `beta` (the
  coefficient), `form` and `heat_maps` (see
  [`randomPlantedForest::prune_components()`](https://plantedml.com/randomPlantedForest/reference/prune_components.html));

- `coarse` and `asis`: for each component of three or more predictors,
  the predictors `C` its heat maps are split by and their cut points
  `cuts`;

- `max_panels`, `intercept`, `mode` and `loss` of the model, and the
  model itself (`model`), which the plots evaluate on grids.

## References

Liu, J., Steensgaard, T., Wright, M. N., Pfister, N., & Hiabu, M.
(2025). *Fast Estimation of Partial Dependence Functions using Trees*.
Proceedings of the 42nd International Conference on Machine Learning,
PMLR 267:39496-39534.
[PMLR](https://proceedings.mlr.press/v267/liu25bm.html) \|
[arXiv:2410.13448](https://arxiv.org/abs/2410.13448)

## Examples

``` r

# Random Planted Forest -----
library(randomPlantedForest)

set.seed(1)
rp <- rpf(mpg ~ ., data = mtcars[1:26, ], max_interaction = 2)

glex_rpf <- glex(rp, mtcars[27:32, ])
str(glex_rpf, list.len = 5)
#> List of 6
#>  $ m          :Classes ‘data.table’ and 'data.frame':    6 obs. of  9 variables:
#>   ..$ cyl : num [1:6] 0.977976 0.977976 -0.711906 -0.000649 -0.711906 ...
#>   ..$ disp: num [1:6] 0.236 0.695 -0.688 0.119 -0.376 ...
#>   ..$ hp  : num [1:6] 0.983 -0.15 -1.144 -0.353 -1.144 ...
#>   ..$ drat: num [1:6] 1.1 -0.22 1.1 -0.287 -0.287 ...
#>   ..$ wt  : num [1:6] 1.1 1.204 0.255 0.328 -0.666 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>  $ intercept  : num 19.9
#>  $ x          :Classes ‘data.table’ and 'data.frame':    6 obs. of  10 variables:
#>   ..$ cyl : num [1:6] 4 4 8 6 8 4
#>   ..$ disp: num [1:6] 120.3 95.1 351 145 301 ...
#>   ..$ hp  : num [1:6] 91 113 264 175 335 109
#>   ..$ drat: num [1:6] 4.43 3.77 4.22 3.62 3.54 4.11
#>   ..$ wt  : num [1:6] 2.14 1.51 3.17 2.77 3.57 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>  $ constrained: chr(0) 
#>  $ shap       :Classes ‘data.table’ and 'data.frame':    6 obs. of  10 variables:
#>   ..$ cyl : num [1:6] 0.977976 0.977976 -0.711906 -0.000649 -0.711906 ...
#>   ..$ disp: num [1:6] 0.236 0.695 -0.688 0.119 -0.376 ...
#>   ..$ hp  : num [1:6] 0.983 -0.15 -1.144 -0.353 -1.144 ...
#>   ..$ drat: num [1:6] 1.1 -0.22 1.1 -0.287 -0.287 ...
#>   ..$ wt  : num [1:6] 1.1 1.204 0.255 0.328 -0.666 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>   [list output truncated]
#>  - attr(*, "class")= chr [1:3] "glex" "rpf_components" "list"

# rpf() above returned a pruned model (randomPlantedForest >= 0.6): its components
glex_rpf$pruned$components
#>   term degree       share beta form heat_maps
#> 1  cyl      1 0.016408116    1 full         1
#> 2 disp      1 0.013502757    1 full         1
#> 3   hp      1 0.017808469    1 full         1
#> 4 drat      1 0.009651168    1 full         1
#> 5   wt      1 0.017818753    1 full         1
#> 6   vs      1 0.005338268    1 full         1
#> 7   am      1 0.011957922    1 full         1
#> 8 gear      1 0.011036225    1 full         1
#> 9 carb      1 0.018822789    1 full         1
# xgboost -----
library(xgboost)
x <- as.matrix(mtcars[, -1])
y <- mtcars$mpg
xg <- xgboost(x[1:26, ], y[1:26],
  max_depth = 4, learning_rate = .1,
  nrounds = 10, verbosity = 0, nthreads = 1
)
glex(xg, x[27:32, ])
#> glex object of subclass xgb_components 
#> Explaining predictions of 6 observations with 31 terms of up to 5 degrees
#> 
#> List of 5
#>  $ shap       :Classes ‘data.table’ and 'data.frame':    6 obs. of  10 variables:
#>   ..$ cyl : num [1:6] 6.70e-02 6.95e-02 1.90e-17 6.95e-02 -2.71e-01 ...
#>   ..$ disp: num [1:6] -0.501 2.445 -0.555 -0.555 -0.278 ...
#>   ..$ hp  : num [1:6] 1.1457 -0.0159 -0.2283 -0.0704 -0.7703 ...
#>   ..$ drat: num [1:6] 0 0 0 0 0 0
#>   ..$ wt  : num [1:6] 0.623 0.809 0.759 0.531 -3.272 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>  $ m          :Classes ‘data.table’ and 'data.frame':    6 obs. of  31 variables:
#>   ..$ cyl                : num [1:6] 0.0742 0.0742 0 0.0742 0 ...
#>   ..$ cyl:disp           : num [1:6] 0 0 0 0 0 0
#>   ..$ cyl:disp:hp        : num [1:6] -8.33e-17 -2.15e-16 1.04e-17 -4.86e-17 1.04e-17 ...
#>   ..$ cyl:disp:hp:wt     : num [1:6] -2.78e-16 8.33e-17 1.11e-16 2.78e-17 5.55e-17 ...
#>   ..$ cyl:disp:wt        : num [1:6] -1.39e-17 -1.39e-17 1.39e-17 -1.39e-17 0.00 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>  $ intercept  : num 20.6
#>  $ x          :Classes ‘data.table’ and 'data.frame':    6 obs. of  10 variables:
#>   ..$ cyl : num [1:6] 4 4 8 6 8 4
#>   ..$ disp: num [1:6] 120.3 95.1 351 145 301 ...
#>   ..$ hp  : num [1:6] 91 113 264 175 335 109
#>   ..$ drat: num [1:6] 4.43 3.77 4.22 3.62 3.54 4.11
#>   ..$ wt  : num [1:6] 2.14 1.51 3.17 2.77 3.57 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>  $ constrained: chr(0) 
#>  - attr(*, "class")= chr [1:3] "glex" "xgb_components" "list"
glex(xg, mtcars[27:32, ])
#> glex object of subclass xgb_components 
#> Explaining predictions of 6 observations with 31 terms of up to 5 degrees
#> 
#> List of 5
#>  $ shap       :Classes ‘data.table’ and 'data.frame':    6 obs. of  11 variables:
#>   ..$ mpg : num [1:6] 0 0 0 0 0 0
#>   ..$ cyl : num [1:6] 6.70e-02 6.95e-02 1.90e-17 6.95e-02 -2.71e-01 ...
#>   ..$ disp: num [1:6] -0.501 2.445 -0.555 -0.555 -0.278 ...
#>   ..$ hp  : num [1:6] 1.1457 -0.0159 -0.2283 -0.0704 -0.7703 ...
#>   ..$ drat: num [1:6] 0 0 0 0 0 0
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>  $ m          :Classes ‘data.table’ and 'data.frame':    6 obs. of  31 variables:
#>   ..$ cyl                : num [1:6] 0.0742 0.0742 0 0.0742 0 ...
#>   ..$ cyl:disp           : num [1:6] 0 0 0 0 0 0
#>   ..$ cyl:disp:hp        : num [1:6] -8.33e-17 -2.15e-16 1.04e-17 -4.86e-17 1.04e-17 ...
#>   ..$ cyl:disp:hp:wt     : num [1:6] -2.78e-16 8.33e-17 1.11e-16 2.78e-17 5.55e-17 ...
#>   ..$ cyl:disp:wt        : num [1:6] -1.39e-17 -1.39e-17 1.39e-17 -1.39e-17 0.00 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>  $ intercept  : num 20.6
#>  $ x          :Classes ‘data.table’ and 'data.frame':    6 obs. of  11 variables:
#>   ..$ mpg : num [1:6] 26 30.4 15.8 19.7 15 21.4
#>   ..$ cyl : num [1:6] 4 4 8 6 8 4
#>   ..$ disp: num [1:6] 120.3 95.1 351 145 301 ...
#>   ..$ hp  : num [1:6] 91 113 264 175 335 109
#>   ..$ drat: num [1:6] 4.43 3.77 4.22 3.62 3.54 4.11
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>  $ constrained: chr(0) 
#>  - attr(*, "class")= chr [1:3] "glex" "xgb_components" "list"

if (FALSE) { # \dontrun{
# Parallel execution
doParallel::registerDoParallel()
glex(xg, x[27:32, ])
} # }
# ranger -----
library(ranger)
x <- as.matrix(mtcars[, -1])
y <- mtcars$mpg
rf <- ranger(
  x = x[1:26, ], y = y[1:26],
  num.trees = 5, max.depth = 3,
  node.stats = TRUE
)
glex(rf, x[27:32, ])
#> glex object of subclass xgb_components 
#> Explaining predictions of 6 observations with 27 terms of up to 4 degrees
#> 
#> List of 5
#>  $ shap       :Classes ‘data.table’ and 'data.frame':    6 obs. of  10 variables:
#>   ..$ cyl : num [1:6] 0.484 0.236 -0.751 0.484 -0.751 ...
#>   ..$ disp: num [1:6] 0.1138 1.6027 -0.9821 0.0938 -0.9821 ...
#>   ..$ hp  : num [1:6] 1.03 1.1 -2.09 0.4 -2.09 ...
#>   ..$ drat: num [1:6] 2.37e-16 2.37e-16 0.00 2.37e-16 0.00 ...
#>   ..$ wt  : num [1:6] 1.43 3.78 -1.13 -1.1 -2.13 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>  $ m          :Classes ‘data.table’ and 'data.frame':    6 obs. of  27 variables:
#>   ..$ disp        : num [1:6] 0.277 1.695 -0.661 0.277 -0.661 ...
#>   ..$ disp:hp     : num [1:6] 0.12 0.12 0.06 0 0.06 ...
#>   ..$ disp:hp:wt  : num [1:6] -1.20e-01 -1.20e-01 1.20e-01 -7.11e-16 1.20e-01 ...
#>   ..$ disp:wt     : num [1:6] -0.142 0 -0.333 -0.142 -0.333 ...
#>   ..$ hp          : num [1:6] 1.366 1.366 -1.89 0.547 -1.89 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>  $ intercept  : num 20.4
#>  $ x          :Classes ‘data.table’ and 'data.frame':    6 obs. of  10 variables:
#>   ..$ cyl : num [1:6] 4 4 8 6 8 4
#>   ..$ disp: num [1:6] 120.3 95.1 351 145 301 ...
#>   ..$ hp  : num [1:6] 91 113 264 175 335 109
#>   ..$ drat: num [1:6] 4.43 3.77 4.22 3.62 3.54 4.11
#>   ..$ wt  : num [1:6] 2.14 1.51 3.17 2.77 3.57 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>  $ constrained: chr(0) 
#>  - attr(*, "class")= chr [1:3] "glex" "xgb_components" "list"
glex(rf, mtcars[27:32, ])
#> glex object of subclass xgb_components 
#> Explaining predictions of 6 observations with 27 terms of up to 4 degrees
#> 
#> List of 5
#>  $ shap       :Classes ‘data.table’ and 'data.frame':    6 obs. of  11 variables:
#>   ..$ mpg : num [1:6] 0 0 0 0 0 0
#>   ..$ cyl : num [1:6] 0.484 0.236 -0.751 0.484 -0.751 ...
#>   ..$ disp: num [1:6] 0.1138 1.6027 -0.9821 0.0938 -0.9821 ...
#>   ..$ hp  : num [1:6] 1.03 1.1 -2.09 0.4 -2.09 ...
#>   ..$ drat: num [1:6] 2.37e-16 2.37e-16 0.00 2.37e-16 0.00 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>  $ m          :Classes ‘data.table’ and 'data.frame':    6 obs. of  27 variables:
#>   ..$ disp        : num [1:6] 0.277 1.695 -0.661 0.277 -0.661 ...
#>   ..$ disp:hp     : num [1:6] 0.12 0.12 0.06 0 0.06 ...
#>   ..$ disp:hp:wt  : num [1:6] -1.20e-01 -1.20e-01 1.20e-01 -7.11e-16 1.20e-01 ...
#>   ..$ disp:wt     : num [1:6] -0.142 0 -0.333 -0.142 -0.333 ...
#>   ..$ hp          : num [1:6] 1.366 1.366 -1.89 0.547 -1.89 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>  $ intercept  : num 20.4
#>  $ x          :Classes ‘data.table’ and 'data.frame':    6 obs. of  11 variables:
#>   ..$ mpg : num [1:6] 26 30.4 15.8 19.7 15 21.4
#>   ..$ cyl : num [1:6] 4 4 8 6 8 4
#>   ..$ disp: num [1:6] 120.3 95.1 351 145 301 ...
#>   ..$ hp  : num [1:6] 91 113 264 175 335 109
#>   ..$ drat: num [1:6] 4.43 3.77 4.22 3.62 3.54 4.11
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5dc767bb1570> 
#>  $ constrained: chr(0) 
#>  - attr(*, "class")= chr [1:3] "glex" "xgb_components" "list"

if (FALSE) { # \dontrun{
# Parallel execution
doParallel::registerDoParallel()
glex(rf, x[27:32, ])
} # }
```
