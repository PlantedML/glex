library(tidyverse)
library(patchwork)
theme_set(theme_bw())


plot_prob_function <- function(coord, ubs) {
  true_cum <- unname(sapply(ubs, function(ub) probFunction(coord, lb = -Inf, ub)))
  emp_cum <- sapply(ubs, function(ub) probFunctionEmp(coord, lb = -Inf, ub))

  combined <- data.frame(ubs, true_cum, emp_cum) |>
    pivot_longer(
      cols = c(true_cum, emp_cum),
      names_to = "type",
      values_to = "value"
    )

  ggplot(data = combined, aes(x = ubs, y = value, color = type)) +
    geom_point()
}

plot_prob_function_diff <- function(coord, ubs) {
  true_cum <- unname(sapply(ubs, function(ub) probFunction(coord, lb = -Inf, ub)))
  emp_cum <- sapply(ubs, function(ub) probFunctionEmp(coord, lb = -Inf, ub))

  combined <- data.frame(ubs = ubs, diff = true_cum - emp_cum)

  ggplot(data = combined, aes(x = ubs, y = diff)) +
    geom_point()
}

plot_shap <- function(object1, object2, coord = 1) {
  x <- dataset$x
  plotdata <- data.frame(
    x1 = x[, 1], x2 = x[, 2],
    shapA1 = true_shap(x[, 1], x[, 2], 1),
    shapA2 = true_shap(x[, 1], x[, 2], 2),
    shapB1 = object1$shap$x1, shapB2 = object1$shap$x2,
    shapEmp1 = object2$shap$x1, shapEmp2 = object2$shap$x2
  )
  plotdata.long <- reshape2::melt(plotdata, id = c("x1", "x2"))


  theme_update(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.6),
    text = element_text(family = "Helvetica Neue")
  )
  p <- ggplot(
    data = plotdata.long[grepl(paste0(coord, "$"), plotdata.long$variable), ],
    aes_string(x = paste0("x", coord), y = "value", color = "variable")
  ) +
    geom_point() +
    labs(y = "SHAP")
  ggsave(paste0("shap", coord, ".png"), plot = p, width = 10, height = 8, dpi = 300)
  p
}

plot_shap_resid <- function(object1, object2, coord = 1, emp_only = F) {
  x <- dataset$x
  resid <- data.frame(
    x = x[, coord],
    resA1 = true_shap(x[, 1], x[, 2], coord) - object2$shap[, ..coord][[1]],
    resB1 = object1$shap[, ..coord][[1]] - object2$shap[, ..coord][[1]]
  ) |> pivot_longer(cols = c("resA1", "resB1"))

  if (emp_only) resid <- resid |> filter(name == "resB1")
  ggplot(data = resid, aes(x = x, y = value, color = name)) +
    geom_point()
}


plot_components <- function(object1, object2, coords = "x1") {
  p1 <- autoplot(object1, coords) + geom_rug(sides = "b")
  p2 <- autoplot(object2, coords) + geom_rug(sides = "b")

  p <- p1 + p2
  ggsave(paste0("components", paste(coords, collapse = ""), ".png"), plot = p, width = 15, height = 8, dpi = 300)
  p
}


plot_components_2 <- function(object1, object2, coords = "x1") {
  o1_m <- object1$m[, ..coords]
  o2_m <- object2$m[, ..coords]

  names(o1_m) <- "o1_m"
  names(o2_m) <- "o2_m"
  combined <- cbind(object1$x, o1_m, o2_m) |>
    pivot_longer(
      cols = c("o1_m", "o2_m"), # Columns to pivot into longer format
      names_to = "variable", # New column for variable names (o1_m, o2_m)
      values_to = "value" # New column for values from o1_m and o2_m
    )

  p <- ggplot(data = combined, aes_string(x = coords, y = "value", color = "variable")) +
    geom_point()

  ggsave(paste0("m", coords, ".png"), plot = p, width = 10, height = 8, dpi = 300)
  p
}


plot_components_diff <- function(object1, object2, coords = "x1") {
  diff <- object1$m[, ..coords] - object2$m[, ..coords]
  names(diff) <- "diff"

  combined <- cbind(object1$x, diff)
  if (coords == "x1" || coords == "x2") {
    aes_mapping <- aes_string(x = coords, y = "diff")
  } else {
    aes_mapping <- aes_string(x = "x1", y = "x2", color = "diff")
  }

  p <- ggplot(data = combined, aes_mapping) +
    geom_point()
  ggsave(paste0("diff_m", coords, ".png"), plot = p, width = 10, height = 8, dpi = 300)
  p
}
