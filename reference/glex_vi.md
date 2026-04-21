# Variable Importance for Main and Interaction Terms

Variable Importance for Main and Interaction Terms

## Usage

``` r
glex_vi(object, ...)
```

## Arguments

- object:

  Object of class `glex`.

- ...:

  (Unused)

## Value

A
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with columns:

- `degree` (`integer`): Degree of interaction of the `term`, with `1`
  being main effects, `2` being 2-degree interactions etc.

- `term` (`character`): Model term, e.g. main effect `x1` or interaction
  term `x1:x2`, `x1:x3:x5` etc.

- `class` (`factor`): For multiclass targets only: The associated target
  class. Lists all classes in the target, not limited to the majority
  vote.

- `m` (`numeric`): Average absolute contribution of `term`, see Details.

- `m_rel` (`numeric`): `m` but relative to the average prediction
  (`intercept` in
  [`glex()`](http://plantedml.com/glex/reference/glex.md) output).

## Details

The `m` reported here is the average absolute value of `m` as reported
by [`glex()`](http://plantedml.com/glex/reference/glex.md), aggregated
by `term`:

\$\$\mathtt{m} = \frac{1}{n} \sum\_{i = 1}^n \|m\| \$\$

In turn, `m_rel` rescales `m` by the average prediction of the model
(\\m_0\\, `intercept` as reported by
[`glex()`](http://plantedml.com/glex/reference/glex.md)):

\$\$\mathtt{m\\rel} = \frac{\mathtt{m}}{m_0}\$\$

## See also

[autoplot.glex_vi](http://plantedml.com/glex/reference/autoplot.glex_vi.md)

## Examples

``` r
set.seed(1)

# xgboost -----
if (requireNamespace("xgboost", quietly = TRUE)) {
library(xgboost)
x <- as.matrix(mtcars[, -1])
y <- mtcars$mpg
xg <- xgboost(data = x[1:26, ], label = y[1:26],
              params = list(max_depth = 4, eta = .1),
              nrounds = 10, verbose = 0)
glex_xgb <- glex(xg, x[27:32, ])
vi_xgb <- glex_vi(glex_xgb)

library(ggplot2)
autoplot(vi_xgb)
autoplot(vi_xgb, by_degree = TRUE)
}
#> Warning: Parameter(s) have been removed from this function: params. This warning will become an error in a future version.
#> Warning: Passed unrecognized parameters: verbose. This warning will become an error in a future version.
#> Warning: Parameter 'data' has been renamed to 'x'. This warning will become an error in a future version.
#> Warning: Parameter 'label' has been renamed to 'y'. This warning will become an error in a future version.
```
