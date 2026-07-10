# Package options for glex plots

The color choices used across all `glex` visualization functions can be
adjusted globally via
[`options()`](https://rdrr.io/r/base/options.html):

## Details

- `glex.palette`:

  (`NULL`) Diverging palette used to color continuous interaction
  effects in
  [`plot_twoway_effects()`](http://plantedml.com/glex/reference/plot_components.md)
  and
  [`plot_threeway_effects()`](http://plantedml.com/glex/reference/plot_components.md).
  The default `NULL` uses a blue/red gradient built from
  `glex.colors_sign`, matching the look of the Python `shap` and
  `shapiq` packages. Set to the name of a diverging
  [scico](https://rdrr.io/pkg/scico/man/scico.html) palette (e.g.
  `"vik"`, `"roma"`) to use that instead.

- `glex.palette_discrete`:

  (`"Dark2"`) Discrete palette used to color categorical predictors in
  interaction plots. Accepts a vector of colors (used via
  [`ggplot2::scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)),
  the string `"okabe-ito"` (the colorblind-safe Okabe-Ito palette via
  [`grDevices::palette.colors()`](https://rdrr.io/r/grDevices/palette.html)),
  the name of a [scico](https://rdrr.io/pkg/scico/man/scico.html)
  palette, or the name of an
  [RColorBrewer](https://ggplot2.tidyverse.org/reference/scale_brewer.html)
  palette.

- `glex.colors_sign`:

  (`c("#008BFB", "#FF0051")`) Two colors for negative and positive
  contributions in
  [`glex_explain()`](http://plantedml.com/glex/reference/glex_explain.md),
  also used as the endpoints of the default continuous gradient. The
  defaults follow the blue/red convention familiar from the Python
  `shap` and `shapiq` packages.

- `glex.color_line`:

  (`"#194155"`) Color for main effect lines and columns drawn by
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  and [`plot_pdp()`](http://plantedml.com/glex/reference/plot_pdp.md).

## Examples

``` r
# Use a scico palette for continuous effects instead of the default gradient
options(glex.palette = "roma")

# Restore the default shap-style gradient
options(glex.palette = NULL)

# Categorical predictors: Okabe-Ito, a scico/brewer palette, or custom colors
options(glex.palette_discrete = "okabe-ito")
options(glex.palette_discrete = "batlow")
options(glex.palette_discrete = c("#E69F00", "#56B4E9", "#009E73"))
```
