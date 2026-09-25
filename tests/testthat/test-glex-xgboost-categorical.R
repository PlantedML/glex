make_cat_data <- function(n = 300, seed = 7) {
  set.seed(seed)
  df <- data.frame(
    age = rnorm(n),
    bmi = rnorm(n),
    cat = factor(sample(c("lo", "mid", "hi", "x"), n, TRUE), levels = c("lo", "mid", "hi", "x"))
  )
  df$y <- as.integer(df$age * (df$cat %in% c("hi", "x")) + 0.5 * df$bmi + rnorm(n, sd = 0.3) > 0)
  df
}

expect_sum_identity <- function(model, x, ...) {
  gl <- suppressMessages(glex(model, x, ...))
  expect_s3_class(gl$x$cat, "factor")
  expect_false(isTRUE(gl$constrained))
  margin <- xgb_margin(model, x)
  expect_equal(unname(gl$intercept + rowSums(gl$m)), unname(margin), tolerance = 1e-5)
  expect_equal(unname(gl$intercept + rowSums(gl$shap)), unname(margin), tolerance = 1e-5)
  gl
}

test_that("categorical xgboost() model decomposes to its margin", {
  df <- make_cat_data()
  x <- df[, c("age", "bmi", "cat")]
  model <- xgboost(x, factor(df$y), nrounds = 20, max_depth = 3, verbosity = 0)

  gl <- expect_sum_identity(model, x)
  expect_true("cat" %in% names(gl$m))
  expect_true(any(grepl("cat", names(gl$m)[get_degree(names(gl$m)) == 2])))
  expect_sum_identity(model, x, weighting_method = "path-dependent")
  # new observations, including levels the model never split on
  expect_sum_identity(model, make_cat_data(seed = 8)[, c("age", "bmi", "cat")])
})

test_that("categorical xgb.train() model on a DMatrix from a data.frame decomposes", {
  df <- make_cat_data()
  x <- df[, c("age", "bmi", "cat")]
  dm <- xgb.DMatrix(x, label = df$y)
  model <- xgb.train(params = list(objective = "binary:logistic", max_depth = 3), data = dm, nrounds = 20)
  expect_sum_identity(model, x)
})

test_that("factor columns plot as categorical", {
  df <- make_cat_data()
  x <- df[, c("age", "bmi", "cat")]
  model <- xgboost(x, factor(df$y), nrounds = 20, max_depth = 3, verbosity = 0)
  gl <- suppressMessages(glex(model, x))

  expect_s3_class(plot_main_effect(gl, "cat"), "ggplot")
  expect_s3_class(plot_twoway_effects(gl, c("age", "cat")), "ggplot")
  expect_s3_class(glex_vi(gl), "glex_vi")
})

test_that("categorical models validate x", {
  df <- make_cat_data()
  x <- df[, c("age", "bmi", "cat")]
  model <- xgboost(x, factor(df$y), nrounds = 5, max_depth = 3, verbosity = 0)

  expect_error(glex(model, as.matrix(transform(x, cat = as.integer(cat)))), "must be a data.frame with factor columns")
  expect_error(glex(model, transform(x, cat = as.integer(cat))), "must be factors: cat")
  expect_error(glex(model, x[, c("age", "bmi")]), "missing from `x`: cat")
  expect_error(glex(model, transform(x, cat = factor(cat, levels = c("lo", "mid")))), "Levels must match")
  expect_error(glex(model, x, weighting_method = "empirical"), "does not support categorical splits")
})
