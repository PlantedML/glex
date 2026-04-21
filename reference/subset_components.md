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

- `subset_components`: An object of class `glex`.

- `subset_component_names`: A character vector.

## Examples

``` r
if (requireNamespace("randomPlantedForest", quietly = TRUE)) {
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

subset_component_names(components, "hp")
}
#>  [1] "hp"          "cyl:hp"      "hp:wt"       "drat:hp"     "hp:vs"      
#>  [6] "cyl:hp:wt"   "cyl:drat:hp" "cyl:hp:vs"   "drat:hp:wt"  "hp:vs:wt"   
#> [11] "drat:hp:vs" 
```
