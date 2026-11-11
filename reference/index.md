# Package index

## Model explanation

Functional decomposition of model predictions

- [`glex()`](https://plantedml.com/glex/reference/glex.md) : Global
  explanations for tree-based models.
- [`glex_vi()`](https://plantedml.com/glex/reference/glex_vi.md) :
  Variable Importance for Main and Interaction Terms

## Visualisation

Visualize main and interaction terms

- [`plot(`*`<glex_vi>`*`)`](https://plantedml.com/glex/reference/plot.glex_vi.md)
  : Plot glex Variable Importances
- [`plot(`*`<glex>`*`)`](https://plantedml.com/glex/reference/plot_components.md)
  [`plot_main_effect()`](https://plantedml.com/glex/reference/plot_components.md)
  [`plot_threeway_effects()`](https://plantedml.com/glex/reference/plot_components.md)
  [`plot_twoway_effects()`](https://plantedml.com/glex/reference/plot_components.md)
  : Plot Prediction Components
- [`plot_force()`](https://plantedml.com/glex/reference/plot_force.md) :
  Force plot of a single prediction
- [`plot_pdp()`](https://plantedml.com/glex/reference/plot_pdp.md) :
  Partial Dependence Plot
- [`plot_shap_decomposition()`](https://plantedml.com/glex/reference/plot_shap_decomposition.md)
  [`glex_explain()`](https://plantedml.com/glex/reference/plot_shap_decomposition.md)
  : Plot how a feature's SHAP value is assembled from components
- [`plot_waterfall()`](https://plantedml.com/glex/reference/plot_waterfall.md)
  : Waterfall plot of a single prediction
- [`glex_options`](https://plantedml.com/glex/reference/glex_options.md)
  : Package options for glex plots

## Utility functions

- [`subset_components()`](https://plantedml.com/glex/reference/subset_components.md)
  [`subset_component_names()`](https://plantedml.com/glex/reference/subset_components.md)
  : Subset components
- [`group_components()`](https://plantedml.com/glex/reference/group_components.md)
  : Group features of a decomposition
- [`dummy_groups()`](https://plantedml.com/glex/reference/dummy_groups.md)
  : Derive feature groups from the factors behind a dummy encoding
- [`print(`*`<glex>`*`)`](https://plantedml.com/glex/reference/print.glex.md)
  : Print glex objects
- [`theme_glex()`](https://plantedml.com/glex/reference/theme_glex.md) :
  A ggplot2 theme for glex plots

## Data

- [`bike`](https://plantedml.com/glex/reference/bike.md) : Bikesharing
  data
