# Hand-built glex objects: plots depend only on the object, so snapshots and
# selection tests stay independent of model fitting and package versions.
fake_glex <- function() {
  x <- data.table::data.table(
    hp = c(110, 264, 93),
    wt = c(2.62, 3.17, 2.32),
    cyl = factor(c(6, 8, 4))
  )
  m <- data.table::data.table(
    hp = c(-0.3, -1.74, 1.29),
    wt = c(0.1, 0.21, 0.83),
    cyl = c(-0.2, -0.49, 1.54),
    `hp:wt` = c(0.05, 0.07, -0.02),
    `cyl:hp` = c(0.02, -0.14, 0.1),
    `cyl:wt` = c(0, 0.04, -0.01),
    `cyl:hp:wt` = c(0.01, -0.03, 0.02)
  )
  structure(
    list(
      shap = data.table::as.data.table(shap_from_components(m, names(x))),
      m = m,
      intercept = 19.56,
      x = x,
      constrained = character(0)
    ),
    class = c("glex", "xgb_components", "list")
  )
}

fake_glex_multiclass <- function() {
  x <- data.table::data.table(hp = c(110, 264), wt = c(2.62, 3.17))
  m <- data.table::data.table(
    `hp__class:a` = c(0.4, -0.3),
    `wt__class:a` = c(-0.1, 0.2),
    `hp:wt__class:a` = c(0.05, -0.02),
    `hp__class:b` = c(-0.4, 0.3),
    `wt__class:b` = c(0.1, -0.2),
    `hp:wt__class:b` = c(-0.05, 0.02)
  )
  structure(
    list(
      m = m,
      intercept = 0.5,
      x = x,
      target_levels = c("a", "b"),
      constrained = character(0)
    ),
    class = c("glex", "rpf_components", "list")
  )
}

fake_glex_many <- function() {
  set.seed(42)
  feats <- paste0("x", 1:6)
  terms <- c(feats, utils::combn(feats, 2, paste, collapse = ":"))
  sds <- rep(c(1, 0.3), c(length(feats), length(terms) - length(feats)))
  m <- data.table::as.data.table(matrix(
    round(stats::rnorm(3 * length(terms), sd = sds), 3),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(NULL, terms)
  ))
  x <- data.table::as.data.table(matrix(
    round(stats::runif(3 * length(feats)), 2),
    nrow = 3,
    dimnames = list(NULL, feats)
  ))
  structure(
    list(
      shap = data.table::as.data.table(shap_from_components(m, feats)),
      m = m,
      intercept = 2,
      x = x,
      constrained = character(0)
    ),
    class = c("glex", "xgb_components", "list")
  )
}
