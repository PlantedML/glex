# Derive feature groups from the factors behind a dummy encoding

Builds the `groups` list for
[`group_components()`](http://plantedml.com/glex/reference/group_components.md)
from the original, un-encoded data: every factor column of `data` whose
encoded level columns are found in `object$x` becomes one group. By
default, level columns are expected under the
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) naming
convention (`paste0(feature, levels)`, e.g. `season` with level
`"Winter"` becomes `"seasonWinter"`), which covers both one-hot
(`~ f - 1`) and treatment (`~ f`) coding – levels without a matching
column (such as a dropped reference level) are simply skipped.

## Usage

``` r
dummy_groups(
  object,
  data,
  naming = function(feature, levels) paste0(feature, levels)
)
```

## Arguments

- object:

  (`glex`) Object of class `glex`.

- data:

  (`data.frame`) The data before dummy encoding; its factor and
  character columns define the candidate groups. Other columns are
  ignored.

- naming:

  (`function(feature, levels)`) Maps a factor name and its levels to the
  encoded column names. Defaults to the
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
  convention `paste0(feature, levels)`.

## Value

Named list of encoded column names, one element per matched factor,
suitable as the `groups` argument of
[`group_components()`](http://plantedml.com/glex/reference/group_components.md).

## Details

If the data was encoded with a different scheme, pass `naming`: a
function of `(feature, levels)` returning the encoded column names, e.g.
`function(feature, levels) paste(feature, levels, sep = "_")`. It must
reproduce the column names the model was trained with, i.e. those found
in `object$x`.

## Examples

``` r
library(xgboost)
set.seed(1)
n <- 200
d <- data.frame(
  f = factor(sample(c("a", "b", "c"), n, replace = TRUE)),
  x1 = rnorm(n)
)
y <- c(a = 0, b = 2, c = -1)[d$f] + d$x1 + rnorm(n)
x <- model.matrix(~ f + x1 - 1, d)

xg <- xgboost(x, y, nrounds = 10, max_depth = 3, nthreads = 1)
gl <- glex(xg, x)

dummy_groups(gl, d)
#> $f
#> [1] "fa" "fb" "fc"
#> 
grouped <- group_components(gl, dummy_groups(gl, d))
names(grouped$m)
#> [1] "f"    "f:x1" "x1"  
```
