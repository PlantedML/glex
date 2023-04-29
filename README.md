
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Global explanations for tree-based models

<!-- badges: start -->

[![R-CMD-check](https://github.com/PlantedML/glex/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PlantedML/glex/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/PlantedML/glex/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PlantedML/glex?branch=master)
<!-- badges: end -->

Global explanations for tree-based models by decomposing regression or
classification functions into the sum of main components and interaction
components of arbitrary order. Calculates exact intervetniaonal SHAP values and q-interaction
interventional SHAP (q being the highest order of interaction present in the model) for tree-based models such as xgboost.

See the accompanying paper for more details and exact definitions: “Unifying local and global model explanations by functional decomposition of low dimensional structures" [(arxiv)](https://arxiv.org/abs/2208.06151).
## Installation

You can install the development version of glex from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("PlantedML/glex")
```

## What’s Included

Currently `glex` works with
[`xgboost`](https://cran.r-project.org/package=xgboost) and
[`randomPlantedForest`](http://plantedml.com/randomPlantedForest/)
models, so we’ll start by fitting a model with one of each:

``` r
# Install xgboost from CRAN
install.packages("xgboost")
# ... and randomPlantedForest from GitHub
remotes::install_github("PlantedML/randomPlantedForest")
```

``` r
library(glex)

# Model fitting
library(randomPlantedForest)
library(xgboost)

# Visualization
library(ggplot2)
library(patchwork)
theme_set(theme_minimal(base_size = 13))

set.seed(21)
```

Note that `xgboost`, unlike `randomPlantedForest`, requires `matrix`
input and does not support categorical predictors.

``` r
rp <- rpf(mpg ~ ., data = mtcars[1:26, ], max_interaction = 3)

x <- as.matrix(mtcars[, -1])
y <- mtcars$mpg
xg <- xgboost(data = x[1:26, ], label = y[1:26],
              params = list(max_depth = 3, eta = .1),
              nrounds = 30, verbose = 0)
```

Using the model objects and a dataset to explain (such as a test set in
this case), we can create `glex` objects for each of the model fits.
These objects of class `glex` are a list containing the prediction
components of main and interaction terms (`$m`), the dataset to be
explained with the observed feature values (`$x`) used to visualize
feature effects, and the average predicted value for the model
(`$intercept`). The `xgboost` method additionally returns the SHAP
values (`$shap`) for each feature in the model.

``` r
glex_rpf <- glex(rp, mtcars[27:32, ])
glex_xgb <- glex(xg, x[27:32, ])
```

Both `m` and `shap` satisfy the property that their sums (per
observation) together with the `intercept` are equal to the model
prediction for each observation:

``` r
# Calculating sum of components and sum of SHAP values
sum_m_rpf <- rowSums(glex_rpf$m) + glex_rpf$intercept
sum_m_xgb <- rowSums(glex_xgb$m) + glex_xgb$intercept
sum_shap_xgb <- rowSums(glex_xgb$shap) + glex_xgb$intercept

# Model predictions
pred_rpf <- predict(rp, mtcars[27:32, ])[[1]]
pred_xgb <- predict(xg, x[27:32, ])

# For XGBoost
cbind(pred_xgb, sum_m_xgb, sum_shap_xgb)
#>      pred_xgb sum_m_xgb sum_shap_xgb
#> [1,] 21.39075  21.39076     21.39076
#> [2,] 20.23664  20.23664     20.23664
#> [3,] 14.73895  14.73895     14.73895
#> [4,] 18.76170  18.76170     18.76170
#> [5,] 13.05614  13.05614     13.05614
#> [6,] 20.23664  20.23664     20.23664

# For RPF
cbind(pred_rpf, sum_m_rpf)
#>      pred_rpf sum_m_rpf
#> [1,] 28.29398  28.29398
#> [2,] 26.02555  26.02555
#> [3,] 18.33365  18.33365
#> [4,] 20.30721  20.30721
#> [5,] 15.22179  15.22179
#> [6,] 24.75446  24.75446
```

### Variable Importances

Variable importance scores are calculated for each main and interaction
term by calculating the average of the absolute prediction components
(`m`) over the dataset supplied to `glex()`.

``` r
vi_rpf <- glex_vi(glex_rpf)
vi_xgb <- glex_vi(glex_xgb)

vi_rpf[1:5, c("degree", "term", "m")]
#>    degree   term         m
#>     <int> <char>     <num>
#> 1:      1     hp 1.3998532
#> 2:      1     wt 1.1725086
#> 3:      1   disp 1.0441441
#> 4:      1   drat 0.8630354
#> 5:      1    cyl 0.5955195
vi_xgb[1:5, c("degree", "term", "m")]
#>    degree   term         m
#>     <int> <char>     <num>
#> 1:      1     hp 1.1360711
#> 2:      1     wt 1.0361386
#> 3:      1   disp 0.6661451
#> 4:      1    cyl 0.5727933
#> 5:      1   qsec 0.2129094
```

The output additionally contains the degree of interaction, which can be
used for filtering and aggregating. Here we filter for terms with
contributions above a `threshold` of `0.05` to get a more compact plot,
with terms below the threshold aggregated into one labelled “Remaining
terms”:

``` r
p_vi1 <- autoplot(vi_rpf, threshold = .05) + 
  labs(title = NULL, tag = "RPF")

p_vi2 <- autoplot(vi_xgb, threshold = .05) + 
  labs(title = NULL, tag = "XGBoost")

p_vi1 + p_vi2 +
  plot_annotation(title = "Variable importance scores by term") & 
  theme(plot.tag.position = "bottomleft")
```

<img src="man/figures/README-glex_vi-plot-1.png" width="100%" /> We can
also sum values within each degree of interaction for a more aggregated
view, which can be useful as it allows us to judge interactions above a
certain degree to not be particularly relevant for a given model.

``` r
p_vi1 <- autoplot(vi_rpf, by_degree = TRUE) + 
  labs(title = NULL, tag = "RPF")

p_vi2 <- autoplot(vi_xgb, by_degree = TRUE) + 
  labs(title = NULL, tag = "XGBoost")

p_vi1 + p_vi2 +
  plot_annotation(title = "Variable importance scores by degree") & 
  theme(plot.tag.position = "top")
```

<img src="man/figures/README-glex_vi-plot-by-degree-1.png" width="100%" />

### Feature Effects

We can also plot prediction components against observed feature values,
which admittedly produces more interesting output with larger, more
interesting datasets.

``` r
p1 <- autoplot(glex_rpf, "hp") + labs(subtitle = "RPF")
p2 <- autoplot(glex_xgb, "hp") + labs(subtitle = "XGBoost")

p1 + p2 + 
  plot_annotation(title = "Main effect for 'hp'")
```

<img src="man/figures/README-plot_components-1.png" width="100%" />

``` r

p1 <- autoplot(glex_rpf, c("hp", "wt")) + labs(subtitle = "RPF")
p2 <- autoplot(glex_xgb, c("hp", "wt")) + labs(subtitle = "XGBoost")

p1 + p2 + 
  plot_annotation(title = "Two-way effects for 'hp' and 'wt'")
```

<img src="man/figures/README-plot_components-2.png" width="100%" />

Currently there is support for plots of interactions up to the third
degree, including continuous and categorical features. Unfortunately,
three-way interactions of continuous features are not supported yet.

### Decomposition of Individual Predictions

Finally, we can explore the prediction for a single observation by
displaying its individual prediction components. The SHAP value is the
sum of all of these components and serves as a reference value. For
compactness, we only plot one feature and collapse all interaction terms
above the second degree into one as their combined effect is very small.

``` r
p1 <- glex_explain(glex_rpf, id = 2, predictors = "hp", max_interaction = 2) + 
  labs(tag = "RPF")
p2 <- glex_explain(glex_xgb, id = 2, predictors = "hp", max_interaction = 2) + 
  labs(tag = "XGBoost")

p1 + p2 & theme(plot.tag.position = "bottom")
```

<img src="man/figures/README-glex_explain-1.png" width="100%" />
