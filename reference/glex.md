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

- `shap`: SHAP values (`xgboost` method only).

- `m`: Functional decomposition into all main and interaction components
  in the model, up to the degree specified by `max_interaction`. The
  variable names correspond to the original variable names, with `:`
  separating interaction terms as one would specify in a
  [`formula`](https://rdrr.io/r/stats/formula.html) interface.

- `intercept`: Intercept term, the expected value of the prediction.

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
#> List of 3
#>  $ m        :Classes ‘data.table’ and 'data.frame':  6 obs. of  55 variables:
#>   ..$ cyl      : num [1:6] 0.787 0.787 -0.581 -0.184 -0.581 ...
#>   ..$ disp     : num [1:6] 0.547 1.803 -0.959 0.343 -0.62 ...
#>   ..$ hp       : num [1:6] 2.839 0.181 -2.506 -0.618 -2.506 ...
#>   ..$ drat     : num [1:6] 2.94 -0.503 2.94 -0.423 -0.423 ...
#>   ..$ wt       : num [1:6] 1.621 1.569 0.465 0.814 -1.043 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  $ intercept: num 20.1
#>  $ x        :Classes ‘data.table’ and 'data.frame':  6 obs. of  10 variables:
#>   ..$ cyl : num [1:6] 4 4 8 6 8 4
#>   ..$ disp: num [1:6] 120.3 95.1 351 145 301 ...
#>   ..$ hp  : num [1:6] 91 113 264 175 335 109
#>   ..$ drat: num [1:6] 4.43 3.77 4.22 3.62 3.54 4.11
#>   ..$ wt  : num [1:6] 2.14 1.51 3.17 2.77 3.57 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  - attr(*, "class")= chr [1:3] "glex" "rpf_components" "list"
# xgboost -----
if (requireNamespace("xgboost", quietly = TRUE)) {
library(xgboost)
x <- as.matrix(mtcars[, -1])
y <- mtcars$mpg
xg <- xgboost(data = x[1:26, ], label = y[1:26],
              params = list(max_depth = 4, eta = .1),
              nrounds = 10, verbose = 0)
glex(xg, x[27:32, ])
glex(xg, mtcars[27:32, ])

if (FALSE) { # \dontrun{
# Parallel execution
doParallel::registerDoParallel()
glex(xg, x[27:32, ])
} # }
}
#> Warning: Parameter(s) have been removed from this function: params. This warning will become an error in a future version.
#> Warning: Passed unrecognized parameters: verbose. This warning will become an error in a future version.
#> Warning: Parameter 'data' has been renamed to 'x'. This warning will become an error in a future version.
#> Warning: Parameter 'label' has been renamed to 'y'. This warning will become an error in a future version.
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
