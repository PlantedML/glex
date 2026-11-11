# Force plot of a single prediction

A compact view of the same information as
[`plot_waterfall()`](https://plantedml.com/glex/reference/plot_waterfall.md):
the components of one observation's prediction as segments of a single
band. Positive terms push the prediction up from the left, negative
terms push it down from the right, and both meet at the prediction f(x).
The largest terms sit next to f(x).

## Usage

``` r
plot_force(
  object,
  id,
  threshold = NULL,
  max_terms = 10L,
  max_interaction = NULL,
  class = NULL
)
```

## Arguments

- object:

  Object of class
  [`glex`](https://plantedml.com/glex/reference/glex.md).

- id:

  (`integer(1)`) Row of `object$x` to explain.

- threshold:

  (`numeric(1): NULL`) Terms with an absolute value at or below this are
  aggregated into one "other terms" bar. The default `NULL` uses 2.5% of
  the largest absolute term, so negligible terms do not crowd the plot;
  set `0` to show all terms.

- max_terms:

  (`integer(1): 10`) Maximum number of terms shown individually, keeping
  the largest in absolute value. The rest are aggregated.

- max_interaction:

  (`integer(1): NULL`) Terms of a higher degree are aggregated.

- class:

  (`character(1): NULL`) For multiclass objects, the class whose
  prediction is shown. Required for multiclass objects.

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Details

Only segments at least 5% as wide as the plotted range are labeled; the
caption states how many are not. Use `max_terms` or `threshold` to
aggregate small terms instead.

The layout follows the force plots introduced by Lundberg et al. (2018)
in the Python 'shap' library, also available in the R package 'shapviz',
which show SHAP values (one segment per feature) rather than the terms
of a functional decomposition.

## References

Lundberg, S. M., Nair, B., Vavilala, M. S., et al. (2018). Explainable
machine-learning predictions for the prevention of hypoxaemia during
surgery. *Nature Biomedical Engineering*, 2, 749–760.
[doi:10.1038/s41551-018-0304-0](https://doi.org/10.1038/s41551-018-0304-0)

Mayer, M. (2026). shapviz: SHAP Visualizations. R package.
<https://CRAN.R-project.org/package=shapviz>

## See also

Other Visualization functions:
[`plot.glex()`](https://plantedml.com/glex/reference/plot_components.md),
[`plot.glex_vi()`](https://plantedml.com/glex/reference/plot.glex_vi.md),
[`plot_pdp()`](https://plantedml.com/glex/reference/plot_pdp.md),
[`plot_shap_decomposition()`](https://plantedml.com/glex/reference/plot_shap_decomposition.md),
[`plot_waterfall()`](https://plantedml.com/glex/reference/plot_waterfall.md)

## Examples

``` r
library(randomPlantedForest)
set.seed(1)
rp <- rpf(mpg ~ ., data = mtcars[1:26, ], max_interaction = 2)
gl <- glex(rp, mtcars[27:32, ])

plot_force(gl, id = 3, max_terms = 6)
```
