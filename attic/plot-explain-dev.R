vi_xgb[, c("degree", "term", "m")] |>
  #rbind(data.table(degree = 0, term = "intercept", m = glex_xgb$intercept)) |>
  ggplot(aes(y = NA, x = m, fill = reorder(term, m))) +
  geom_col(position = "stack", width = 1) +
  geom_label(
    aes(x = cumsum(m), label = round(m, 2)),
    nudge_x = -0.1,
    show.legend = FALSE
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank()
  )


glex_explain(components, 132, threshold = 5)
