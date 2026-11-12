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

# Fit the model, get components
set.seed(12)
rpfit <- rpf(mpg ~ cyl + wt + hp + drat + vs, data = mtcars, max_interaction = 3)
components <- glex(rpfit, mtcars)

# Get component object with only "hp" and its interactions
subset_components(components, "hp")
#> glex object of subclass rpf_components 
#> Explaining predictions of 32 observations with 1 term of up to 1 degree
#> 
#> List of 6
#>  $ m          :Classes ‘data.table’ and 'data.frame':    32 obs. of  1 variable:
#>   ..$ hp: num [1:32] 0.287 0.287 0.984 0.287 -0.625 ...
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x56300c028a30> 
#>  $ intercept  : num 20.2
#>  $ x          :Classes ‘data.table’ and 'data.frame':    32 obs. of  1 variable:
#>   ..$ hp: num [1:32] 110 110 93 110 175 105 245 62 95 123 ...
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x56300c028a30> 
#>  $ constrained: chr(0) 
#>  $ shap       :Classes ‘data.table’ and 'data.frame':    32 obs. of  5 variables:
#>   ..$ cyl : num [1:32] -0.365 -0.365 1.954 -0.365 -1.531 ...
#>   ..$ wt  : num [1:32] 0.403 -0.166 1.253 -0.528 -0.993 ...
#>   ..$ hp  : num [1:32] 0.287 0.287 0.984 0.287 -0.625 ...
#>   ..$ drat: num [1:32] 0.1785 0.1785 0.0332 -0.5561 -0.5172 ...
#>   ..$ vs  : num [1:32] -0.65 -0.65 0.723 0.723 -0.65 ...
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x56300c028a30> 
#>   [list output truncated]
#>  - attr(*, "class")= chr [1:3] "glex" "rpf_components" "list"

subset_component_names(components, "hp")
#> [1] "hp"
```
