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
library(randomPlantedForest)
rp <- rpf(mpg ~ hp + wt + drat, data = mtcars[1:26, ], max_interaction = 2)

glex(rp, mtcars[27:32, ])
#> glex object of subclass rpf_components 
#> Explaining predictions of 6 observations with 3 terms of up to 1 degree
#> 
#> List of 6
#>  $ m          :Classes ‘data.table’ and 'data.frame':    6 obs. of  3 variables:
#>   ..$ hp  : num [1:6] 5.241 0.247 -4.672 -1.878 -4.672 ...
#>   ..$ wt  : num [1:6] 0.0929 0.1054 0.028 0.0369 -0.0461 ...
#>   ..$ drat: num [1:6] 6.785 -0.755 6.785 -1.528 -1.528 ...
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x56300c028a30> 
#>  $ intercept  : num 20
#>  $ x          :Classes ‘data.table’ and 'data.frame':    6 obs. of  3 variables:
#>   ..$ hp  : num [1:6] 91 113 264 175 335 109
#>   ..$ wt  : num [1:6] 2.14 1.51 3.17 2.77 3.57 ...
#>   ..$ drat: num [1:6] 4.43 3.77 4.22 3.62 3.54 4.11
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x56300c028a30> 
#>  $ constrained: chr(0) 
#>  $ shap       :Classes ‘data.table’ and 'data.frame':    6 obs. of  3 variables:
#>   ..$ hp  : num [1:6] 5.241 0.247 -4.672 -1.878 -4.672 ...
#>   ..$ wt  : num [1:6] 0.0929 0.1054 0.028 0.0369 -0.0461 ...
#>   ..$ drat: num [1:6] 6.785 -0.755 6.785 -1.528 -1.528 ...
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x56300c028a30> 
#>   [list output truncated]
#>  - attr(*, "class")= chr [1:3] "glex" "rpf_components" "list"
```
