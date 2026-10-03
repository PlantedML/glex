# Group features of a decomposition

Aggregates the terms of a decomposition such that a set of features is
treated as a single feature, named after the group. All terms involving
only members of a group are summed into the group's main effect, and
terms mixing group members with other features become interactions of
the group. Since the decomposition is additive, this regrouping is
exact: components still sum to the model prediction, and SHAP values of
a group are the sums of its members' SHAP values, preserving the
efficiency property.

## Usage

``` r
group_components(object, groups)
```

## Arguments

- object:

  (`glex`) Object of class `glex`.

- groups:

  (`list`) Named list of character vectors of feature names in
  `object$x`. Each vector becomes a single feature named after the list
  element, e.g. `list(f = c("fa", "fb", "fc"))`. Features may appear in
  at most one group; features not listed remain unchanged.

## Value

Object of class `glex` with grouped `m`, `shap` and `x`.

## Details

The motivating use case is dummy-encoded categorical features in
`xgboost`: the decomposition of a one-hot encoded factor spreads its
effect across the dummies and their interactions (e.g. `fa:fb`), which
are artifacts of the encoding rather than meaningful interactions.
Grouping the dummies restores the factor as one feature, e.g.
`groups = list(f = c("fa", "fb", "fc"))` turns `fa`, `fb`, `fc`,
`fa:fb`, ... into `f`, and `fa:x1`, `fa:fb:x1`, ... into `f:x1`.

For the grouped columns of `x`, a factor is reconstructed when the
group's columns form a dummy encoding (0/1 values, at most one active
per row), with the active column's name as the level and `"(base)"` for
rows with no active column (treatment coding). Otherwise the group's
column in `x` is `NA` and plot functions cannot be used for that group's
main effect.

## Examples

``` r
library(xgboost)
set.seed(1)
n <- 200
f <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
x1 <- rnorm(n)
y <- c(a = 0, b = 2, c = -1)[f] + x1 + c(a = 1, b = 0, c = -1)[f] * x1 + rnorm(n)
x <- cbind(model.matrix(~ f - 1), x1)

xg <- xgboost(x, y, nrounds = 10, max_depth = 3, nthreads = 1)
gl <- glex(xg, x)
names(gl$m)
#>  [1] "fa"       "fa:fb"    "fa:fb:x1" "fa:x1"    "fb"       "fb:x1"   
#>  [7] "x1"       "fa:fc"    "fa:fc:x1" "fc"       "fc:x1"    "fb:fc"   
#> [13] "fb:fc:x1"

grouped <- group_components(gl, groups = list(f = c("fa", "fb", "fc")))
names(grouped$m)
#> [1] "f"    "f:x1" "x1"  
all.equal(rowSums(grouped$m), rowSums(gl$m))
#> [1] TRUE
```
