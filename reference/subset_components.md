# Subset components

Subset components

## Usage

``` r
subset_components(components, term)

subset_component_names(components, term)
```

## Arguments

- components:

  An object of class `glex`.

- term:

  (`character(1)`) A main term name to subset by, e.g. `"x1"`.

## Value

- `subset_components()`: An object of class `glex`.

- `subset_component_names()`: A character vector.

## Examples

``` r
library(randomPlantedForest)

# introduce factor variables to show categorical feature handling
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)

# Fit forest, get components
set.seed(12)
rpfit <- rpf(mpg ~ cyl + wt + hp + drat + vs, data = mtcars, ntrees = 25, max_interaction = 3)
components <- glex(rpfit, mtcars)

# Get component object with only "hp" and its interactions
subset_components(components, "hp")
#> glex object of subclass rpf_components 
#> Explaining predictions of 32 observations with 11 terms of up to 3 degrees
#> 
#> List of 5
#>  $ m          :Classes ‘data.table’ and 'data.frame':    32 obs. of  11 variables:
#>   ..$ hp         : num [1:32] 0.718 0.718 0.972 0.718 -0.311 ...
#>   ..$ cyl:hp     : num [1:32] 0.1485 0.1485 0.6333 0.1485 -0.0335 ...
#>   ..$ hp:wt      : num [1:32] 0.0388 0.0489 0.015 0.0776 0.2166 ...
#>   ..$ drat:hp    : num [1:32] 0.1319 0.1319 0.1997 0.0311 0.3555 ...
#>   ..$ hp:vs      : num [1:32] -0.00516 -0.00516 0.20398 -0.03087 -0.00649 ...
#>   .. [list output truncated]
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5632b811ca10> 
#>  $ intercept  : num 16.7
#>  $ x          :Classes ‘data.table’ and 'data.frame':    32 obs. of  1 variable:
#>   ..$ hp: num [1:32] 110 110 93 110 175 105 245 62 95 123 ...
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5632b811ca10> 
#>  $ constrained: chr(0) 
#>  $ shap       :Classes ‘data.table’ and 'data.frame':    32 obs. of  5 variables:
#>   ..$ cyl : num [1:32] 2.59 2.58 4.29 2.59 1.05 ...
#>   ..$ wt  : num [1:32] -0.0999 -0.2674 2.3393 -0.3135 -0.8036 ...
#>   ..$ hp  : num [1:32] 0.853 0.859 1.381 0.764 0.103 ...
#>   ..$ drat: num [1:32] 0.245 0.247 0.172 0.236 0.341 ...
#>   ..$ vs  : num [1:32] 0.727 0.72 0.98 0.876 0.68 ...
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5632b811ca10> 
#>  - attr(*, "class")= chr [1:3] "glex" "rpf_components" "list"

subset_component_names(components, "hp")
#>  [1] "hp"          "cyl:hp"      "hp:wt"       "drat:hp"     "hp:vs"      
#>  [6] "cyl:hp:wt"   "cyl:drat:hp" "cyl:hp:vs"   "drat:hp:wt"  "hp:vs:wt"   
#> [11] "drat:hp:vs" 
```
