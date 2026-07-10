test_that("max_interaction respects xgb's max_depth", {
  set.seed(1)
  x <- as.matrix(mtcars[, -1])

  xg <- xgboost(x, mtcars$mpg, nrounds = 50, verbose = 0, max_depth = 2)
  glexb <- glex(xg, x)
  max_degree <- max(lengths(strsplit(names(glexb$m), split = ":", fixed = TRUE)))

  # The maximum interaction degree is 2^d - 1 = 4 - 1 = 3
  expect_equal(max_degree, 3)
})

test_that("features argument only calculates for given features", {
  x <- as.matrix(mtcars[, -1])
  xg <- xgboost(x, mtcars$mpg, nrounds = 10, verbose = 0)
  glexb <- glex(xg, x, features = c("cyl", "disp"))
  expect_equal(colnames(glexb$m), c("cyl", "cyl:disp", "disp"))
})

test_that("features argument results in same values as without", {
  x <- as.matrix(mtcars[, -1])
  xg <- xgboost(x, mtcars$mpg, nrounds = 10, verbose = 0)
  glexb1 <- glex(xg, x, features = c("cyl", "disp"))
  glexb2 <- glex(xg, x)
  cols <- c("cyl", "disp", "cyl:disp")
  expect_equal(glexb1$m[, ..cols], glexb2$m[, ..cols])
})

test_that("features argument works together with max_interaction", {
  x <- as.matrix(mtcars[, -1])
  xg <- xgboost(x, mtcars$mpg, nrounds = 10, verbose = 0)
  glexb <- glex(xg, x, features = c("cyl", "disp"), max_interaction = 1)
  expect_equal(colnames(glexb$m), c("cyl", "disp"))
})


x_train <- as.matrix(mtcars[1:26, -1])
x_test <- as.matrix(mtcars[27:32, -1])
y_train <- mtcars$mpg[1:26]
y_test <- mtcars$mpg[27:32]

xg <- xgboost(x_train, y_train, nrounds = 10, max_depth = 4, verbose = 0)
pred_train <- predict(xg, x_train)
pred_test <- predict(xg, x_test)

res_train <- glex(xg, x_train, max_interaction = 4, weighting_method = "fastpd")
res_test <- glex(xg, x_test, max_interaction = 4, weighting_method = "fastpd")

res_train_path <- glex(xg, x_train, max_interaction = 4, weighting_method = "path-dependent")
res_test_path <- glex(xg, x_test, max_interaction = 4, weighting_method = "path-dependent")


test_that("FastPD xgboost prediction is approx. same as sum of shap + intercept, training data", {
  expect_equal(unname(res_train$intercept + rowSums(res_train$shap)),
    unname(pred_train),
    tolerance = 1e-5
  )
})

test_that("FastPD xgboost prediction is approx. same as sum of shap + intercept, test data", {
  expect_equal(unname(res_test$intercept + rowSums(res_test$shap)),
    unname(pred_test),
    tolerance = 1e-5
  )
})

test_that("FastPD xgboost prediction is approx. same as sum of decomposition + intercept, training data", {
  expect_equal(unname(res_train$intercept + rowSums(res_train$m)),
    unname(pred_train),
    tolerance = 1e-5
  )
})

test_that("FastPD xgboost prediction is approx. same as sum of decomposition + intercept, test data", {
  expect_equal(unname(res_test$intercept + rowSums(res_test$m)),
    unname(pred_test),
    tolerance = 1e-5
  )
})


test_that("Path-dependent xgboost prediction is approx. same as sum of shap + intercept, training data", {
  expect_equal(unname(res_train_path$intercept + rowSums(res_train_path$shap)),
    unname(pred_train),
    tolerance = 1e-5
  )
})

test_that("Path-dependent xgboost prediction is approx. same as sum of shap + intercept, test data", {
  expect_equal(unname(res_test_path$intercept + rowSums(res_test_path$shap)),
    unname(pred_test),
    tolerance = 1e-5
  )
})

test_that("Path-dependent xgboost prediction is approx. same as sum of decomposition + intercept, training data", {
  expect_equal(unname(res_train_path$intercept + rowSums(res_train_path$m)),
    unname(pred_train),
    tolerance = 1e-5
  )
})

test_that("Path-dependent xgboost prediction is approx. same as sum of decomposition + intercept, test data", {
  expect_equal(unname(res_test_path$intercept + rowSums(res_test_path$m)),
    unname(pred_test),
    tolerance = 1e-5
  )
})

test_that("xgboost models with different base_score values are reconstructed correctly", {
  x_train <- as.matrix(mtcars[1:26, -1])
  x_test <- as.matrix(mtcars[27:32, -1])
  y_train <- mtcars$mpg[1:26]

  base_scores <- c(0.5, 5, 20)
  dtrain <- xgboost::xgb.DMatrix(data = x_train, label = y_train)

  for (bs in base_scores) {
    xg_bs <- xgboost::xgb.train(
      params = list(
        objective = "reg:squarederror",
        max_depth = 4,
        eta = 0.1,
        base_score = bs
      ),
      data = dtrain,
      nrounds = 10,
      verbose = 0
    )

    pred_bs <- predict(xg_bs, x_test)
    res_bs <- glex(xg_bs, x_test, max_interaction = 4, weighting_method = "fastpd")

    expect_equal(
      get_xgb_base_score(xg_bs),
      bs,
      tolerance = 1e-10,
      info = paste0("base_score extraction failed for base_score=", bs)
    )

    expect_equal(
      unname(res_bs$intercept + rowSums(res_bs$shap)),
      unname(pred_bs),
      tolerance = 1e-5,
      info = paste0("reconstruction failed for base_score=", bs)
    )
  }
})

test_that("xgboost reg:gamma (log link) is reconstructed on margin and response scales", {
  set.seed(42)
  x <- matrix(rnorm(400), ncol = 4)
  colnames(x) <- paste0("x", 1:4)
  y <- exp(0.2 + x[, 1] - 0.5 * x[, 2] + rnorm(nrow(x), sd = 0.1))

  x_train <- x[1:80, , drop = FALSE]
  x_test <- x[81:100, , drop = FALSE]
  y_train <- y[1:80]

  dtrain <- xgboost::xgb.DMatrix(data = x_train, label = y_train)
  xg_gamma <- xgboost::xgb.train(
    params = list(
      objective = "reg:gamma",
      max_depth = 3,
      eta = 0.1
    ),
    data = dtrain,
    nrounds = 30,
    verbose = 0
  )

  pred_margin <- predict(xg_gamma, x_test, outputmargin = TRUE)
  pred_response <- predict(xg_gamma, x_test)
  res <- glex(xg_gamma, x_test, weighting_method = "fastpd")

  margin_from_shap <- unname(res$intercept + rowSums(res$shap))
  margin_from_m <- unname(res$intercept + rowSums(res$m))
  response_from_shap <- exp(margin_from_shap)
  response_from_m <- exp(margin_from_m)

  expect_equal(margin_from_shap, unname(pred_margin), tolerance = 1e-5)
  expect_equal(margin_from_m, unname(pred_margin), tolerance = 1e-5)
  expect_equal(response_from_shap, unname(pred_response), tolerance = 1e-5)
  expect_equal(response_from_m, unname(pred_response), tolerance = 1e-5)
})
