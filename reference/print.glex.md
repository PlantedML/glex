# Print glex objects

This is implemented mainly to avoid flooding the console in cases where
the `glex` object uses many terms, which leads to a large amount of
column names of `$m` being printed to the console. This function wraps
[`str()`](https://rdrr.io/r/utils/str.html) with a truncated output for
a more compact representation.

## Usage

``` r
# S3 method for class 'glex'
print(x, ...)
```

## Arguments

- x:

  Object to print.

- ...:

  (Unused)

## Examples

``` r
# Random Planted Forest -----
if (requireNamespace("randomPlantedForest", quietly = TRUE)) {
library(randomPlantedForest)
rp <- rpf(mpg ~ hp + wt + drat, data = mtcars[1:26, ], max_interaction = 2)

glex(rp, mtcars[27:32, ])
}
#> glex object of subclass rpf_components 
#> Explaining predictions of 6 observations with 6 terms of up to 2 degrees
#> 
#> List of 3
#>  $ m        :Classes ‘data.table’ and 'data.frame':  6 obs. of  6 variables:
#>   ..$ hp     : num [1:6] 4.573 0.317 -3.868 -0.902 -3.868 ...
#>   ..$ wt     : num [1:6] 4.501 4.27 0.966 0.988 -1.58 ...
#>   ..$ drat   : num [1:6] 2.909 -0.061 2.909 -0.144 -0.144 ...
#>   ..$ hp:wt  : num [1:6] 0.21143 0.00946 -0.00105 -0.29992 -0.16586 ...
#>   ..$ drat:hp: num [1:6] 0.2645 -0.0681 0.1689 0.0802 -0.0323 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  $ intercept: num 20.2
#>  $ x        :Classes ‘data.table’ and 'data.frame':  6 obs. of  3 variables:
#>   ..$ hp  : num [1:6] 91 113 264 175 335 109
#>   ..$ wt  : num [1:6] 2.14 1.51 3.17 2.77 3.57 ...
#>   ..$ drat: num [1:6] 4.43 3.77 4.22 3.62 3.54 4.11
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  - attr(*, "class")= chr [1:3] "glex" "rpf_components" "list"
```
