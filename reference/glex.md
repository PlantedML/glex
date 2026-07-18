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

  Data to be explained.

- max_interaction:

  (`integer(1): NULL`)  
  Maximum interaction size to consider. Defaults to using all possible
  interactions available in the model.  
  For [`xgboost`](https://rdrr.io/pkg/xgboost/man/xgb.train.html), this
  defaults to the `max_depth` parameter of the model fit.  
  If not set in `xgboost`, the default value of `6` is assumed.

- features:

  Vector of column names in x to calculate components for. Default is
  `NULL`, i.e. all features are used.

- ...:

  Further arguments passed to methods.

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
  models, columns are class-specific like those of `m`. Note that
  `randomPlantedForest` models report a single `intercept` for all
  classes, so for multiclass models `intercept + rowSums(shap)`
  reconstructs the predicted class scores only approximately.

- `m`: Functional decomposition into all main and interaction components
  in the model, up to the degree specified by `max_interaction`. The
  variable names correspond to the original variable names, with `:`
  separating interaction terms as one would specify in a
  [`formula`](https://rdrr.io/r/stats/formula.html) interface.

- `intercept`: Intercept term, the expected value of the prediction.

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

The different weighting methods are described in detail in Liu et al.
(2024). The default method is "fastpd" as it consistently estimates the
correct partial dependence function.

## References

Liu, J., Steensgaard, T., Wright, M. N., Pfister, N., & Hiabu, M.
(2024). *Fast Estimation of Partial Dependence Functions using Trees*.
arXiv preprint [arXiv:2410.13448](https://arxiv.org/abs/2410.13448).

## Examples

``` r

# Random Planted Forest -----
if (requireNamespace("randomPlantedForest", quietly = TRUE)) {
library(randomPlantedForest)

rp <- rpf(mpg ~ ., data = mtcars[1:26, ], max_interaction = 2)

glex_rpf <- glex(rp, mtcars[27:32, ])
str(glex_rpf, list.len = 5)
}
#> List of 5
#>  $ m          :Classes ‘data.table’ and 'data.frame':    6 obs. of  55 variables:
#>   ..$ cyl      : num [1:6] 0.773 0.773 -0.352 0.245 -0.352 ...
#>   ..$ disp     : num [1:6] 0.695 1.276 -1.09 0.406 -0.94 ...
#>   ..$ hp       : num [1:6] 2.95 4.32e-05 -2.32 -1.02 -2.32 ...
#>   ..$ drat     : num [1:6] 1.516 -0.143 1.516 -0.37 -0.37 ...
#>   ..$ wt       : num [1:6] 1.62 1.763 0.593 0.567 -0.801 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x56058db34a10> 
#>  $ intercept  : num 19.8
#>  $ x          :Classes ‘data.table’ and 'data.frame':    6 obs. of  10 variables:
#>   ..$ cyl : num [1:6] 4 4 8 6 8 4
#>   ..$ disp: num [1:6] 120.3 95.1 351 145 301 ...
#>   ..$ hp  : num [1:6] 91 113 264 175 335 109
#>   ..$ drat: num [1:6] 4.43 3.77 4.22 3.62 3.54 4.11
#>   ..$ wt  : num [1:6] 2.14 1.51 3.17 2.77 3.57 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x56058db34a10> 
#>  $ constrained: chr(0) 
#>  $ shap       :Classes ‘data.table’ and 'data.frame':    6 obs. of  10 variables:
#>   ..$ cyl : num [1:6] 0.767 0.719 -0.35 0.334 -0.356 ...
#>   ..$ disp: num [1:6] 0.715 1.293 -1.03 0.501 -0.879 ...
#>   ..$ hp  : num [1:6] 3.16 0.179 -2.187 -1.025 -2.175 ...
#>   ..$ drat: num [1:6] 1.656 -0.118 1.611 -0.384 -0.35 ...
#>   ..$ wt  : num [1:6] 1.751 1.966 0.656 0.545 -0.791 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x56058db34a10> 
#>  - attr(*, "class")= chr [1:3] "glex" "rpf_components" "list"
# xgboost -----
if (requireNamespace("xgboost", quietly = TRUE)) {
library(xgboost)
x <- as.matrix(mtcars[, -1])
y <- mtcars$mpg
xg <- xgboost(x[1:26, ], y[1:26],
              max_depth = 4, learning_rate = .1,
              nrounds = 10, verbosity = 0, nthreads = 1)
glex(xg, x[27:32, ])
glex(xg, mtcars[27:32, ])

if (FALSE) { # \dontrun{
# Parallel execution
doParallel::registerDoParallel()
glex(xg, x[27:32, ])
} # }
}
# ranger -----
if (requireNamespace("ranger", quietly = TRUE)) {
library(ranger)
x <- as.matrix(mtcars[, -1])
y <- mtcars$mpg
rf <- ranger(x = x[1:26, ], y = y[1:26],
             num.trees = 5, max.depth = 3,
             node.stats = TRUE)
glex(rf, x[27:32, ])
glex(rf, mtcars[27:32, ])

if (FALSE) { # \dontrun{
# Parallel execution
doParallel::registerDoParallel()
glex(rf, x[27:32, ])
} # }
}
```
