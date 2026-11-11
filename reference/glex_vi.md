# Variable Importance for Main and Interaction Terms

Variable Importance for Main and Interaction Terms

## Usage

``` r
glex_vi(object, measure = c("mean_abs", "variance", "share"), ...)
```

## Arguments

- object:

  Object of class `glex`.

- measure:

  (`character(1): "mean_abs"`) How a term's importance is measured, see
  Details:

  - `"mean_abs"`: its mean absolute value at `x`.

  - `"variance"`: its mean square at `x`, and as a share of the variance
    of the prediction.

  - `"share"`: for the
    [`glex()`](https://plantedml.com/glex/reference/glex.md) object of a
    pruned Random Planted Forest only: the pruning's own share of the
    variance of the prediction on the training rows
    (`object$pruned$components$share`), by which the pruning chose the
    terms.

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

- `class` (`factor`): For multiclass targets and `measure = "mean_abs"`
  only: The associated target class. Lists all classes in the target,
  not limited to the majority vote.

- `m` (`numeric`): The importance of `term` by `measure`, see Details.

- `m_rel` (`numeric`): `m` relative to a scale, see Details.

The `measure` is stored as the attribute `"measure"`. Terms with `m` of
zero are left out; the rows are sorted by decreasing `m`.

## Details

**`measure = "mean_abs"`** (the default). The `m` reported is the
average absolute value of `m` as reported by
[`glex()`](https://plantedml.com/glex/reference/glex.md), aggregated by
`term` (and by class for multiclass models):

\$\$\mathtt{m} = \frac{1}{n} \sum\_{i = 1}^n \|m_S(x_i)\| \$\$

In turn, `m_rel` rescales `m` by the average prediction of the model
(\\m_0\\, `intercept` as reported by
[`glex()`](https://plantedml.com/glex/reference/glex.md); its class's
for models with one intercept per class):

\$\$\mathtt{m\\rel} = \frac{\mathtt{m}}{m_0}\$\$

It is in units of the prediction: how far the term moves a typical
prediction.

**`measure = "variance"`.** `m` is the mean square of the term,

\$\$\mathtt{m} = \frac{1}{n} \sum\_{i = 1}^n m_S(x_i)^2, \$\$

which is the mean squared change of the prediction if the term were
dropped: what dropping it costs in squared error. `m_rel` is `m` over
the variance of the prediction at `x` (the intercept plus all terms, and
`remainder` if any), its share of the variance of the prediction. For
the components of a purified decomposition (mean zero), `m` is the
variance of the term. For multiclass models, `m` and the variance of the
prediction are summed over the classes' scores (one row per term). This
is the size by which randomPlantedForest's
[`prune_components()`](https://plantedml.com/randomPlantedForest/reference/prune_components.html)
ranks and prunes the components, except that it measures it on the
training rows, and for classification with `loss = "logit"` or
`"exponential"` centres the scores across the classes first: for the
[`glex()`](https://plantedml.com/glex/reference/glex.md) object of a
pruned model, `measure = "share"` gives the pruning's own numbers.

**`measure = "share"`.** For the
[`glex()`](https://plantedml.com/glex/reference/glex.md) object of a
pruned Random Planted Forest: `m` and `m_rel` are both the pruning's
share of the variance of the prediction on the training rows, as in
`object$pruned$components`.

The two measures can rank terms differently: a term that is large on a
few observations and zero elsewhere ranks higher by its variance than by
its mean absolute value.

## See also

[`plot.glex_vi()`](https://plantedml.com/glex/reference/plot.glex_vi.md)

## Examples

``` r
set.seed(1)

# xgboost -----
library(xgboost)
x <- as.matrix(mtcars[, -1])
y <- mtcars$mpg
xg <- xgboost(x[1:26, ], y[1:26],
  max_depth = 4, learning_rate = .1,
  nrounds = 10, verbosity = 0, nthreads = 1
)
glex_xgb <- glex(xg, x[27:32, ])
vi_xgb <- glex_vi(glex_xgb)

plot(vi_xgb)

plot(vi_xgb, by_degree = TRUE)


# by the share of the variance of the prediction
plot(glex_vi(glex_xgb, measure = "variance"), scale = "relative")
```
