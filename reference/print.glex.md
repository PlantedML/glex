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
#> List of 5
#>  $ m          :Classes ‘data.table’ and 'data.frame':    6 obs. of  6 variables:
#>   ..$ hp     : num [1:6] 4.206 0.391 -3.793 -0.703 -3.793 ...
#>   ..$ wt     : num [1:6] 3.62 3.93 1.19 1.12 -1.76 ...
#>   ..$ drat   : num [1:6] 2.3367 -0.1929 2.3367 -0.0693 -0.0693 ...
#>   ..$ hp:wt  : num [1:6] -0.597 0.0628 0.1089 -0.2085 -0.0806 ...
#>   ..$ drat:hp: num [1:6] 0.16942 -0.00845 0.2715 0.03612 0.03014 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x56058db34a10> 
#>  $ intercept  : num 19.9
#>  $ x          :Classes ‘data.table’ and 'data.frame':    6 obs. of  3 variables:
#>   ..$ hp  : num [1:6] 91 113 264 175 335 109
#>   ..$ wt  : num [1:6] 2.14 1.51 3.17 2.77 3.57 ...
#>   ..$ drat: num [1:6] 4.43 3.77 4.22 3.62 3.54 4.11
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x56058db34a10> 
#>  $ constrained: chr(0) 
#>  $ shap       :Classes ‘data.table’ and 'data.frame':    6 obs. of  3 variables:
#>   ..$ hp  : num [1:6] 3.993 0.418 -3.603 -0.789 -3.819 ...
#>   ..$ wt  : num [1:6] 3.35 3.945 1.291 0.997 -1.791 ...
#>   ..$ drat: num [1:6] 2.4518 -0.2115 2.5208 -0.0748 -0.048 ...
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x56058db34a10> 
#>  - attr(*, "class")= chr [1:3] "glex" "rpf_components" "list"
```
