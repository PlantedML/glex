same_as_dt_tree <- function(model) {
  reference <- xgboost::xgb.model.dt.tree(model = model, use_int_id = TRUE)
  parsed <- xgb_trees(model)$trees
  for (col in c("Tree", "Node", "Feature", "Yes", "No", "Missing")) {
    expect_identical(parsed[[col]], reference[[col]], label = col)
  }
  for (col in c("Split", "Gain", "Cover")) {
    expect_equal(parsed[[col]], reference[[col]], tolerance = 1e-6, label = col)
  }
  expect_true(all(lengths(parsed$Categories) == 0))
}

test_that("JSON tree parser matches xgb.model.dt.tree for numeric models", {
  x <- as.matrix(mtcars[, -1])
  same_as_dt_tree(xgboost(x, mtcars$mpg, nrounds = 5, max_depth = 3, verbosity = 0))
  same_as_dt_tree(xgboost(x, factor(mtcars$cyl), nrounds = 3, max_depth = 2, verbosity = 0))
  # unnamed matrix: features referred to by index
  same_as_dt_tree(xgboost(unname(x), mtcars$mpg, nrounds = 3, max_depth = 2, verbosity = 0))
})

test_that("JSON tree parser exposes categorical splits", {
  set.seed(1)
  n <- 200
  df <- data.frame(age = rnorm(n), cat = factor(sample(c("lo", "mid", "hi"), n, TRUE)))
  y <- as.integer(df$age * (df$cat == "hi") > 0)
  model <- xgboost(df, factor(y), nrounds = 3, max_depth = 2, verbosity = 0)

  parsed <- xgb_trees(model)
  expect_identical(parsed$categorical, "cat")
  cat_nodes <- parsed$trees[Feature == "cat"]
  expect_gt(nrow(cat_nodes), 0)
  expect_true(all(lengths(cat_nodes$Categories) > 0))
  expect_true(all(unlist(cat_nodes$Categories) %in% 0:2))
  # members of the set go to the right child, which is `Yes`
  dump <- xgboost::xgb.dump(model)
  first <- parsed$trees[Feature == "cat"][1]
  expect_match(dump[grep(sprintf("^%d:\\[cat:", first$Node), dump)][1], sprintf("yes=%d,no=%d", first$Yes, first$No))
})
