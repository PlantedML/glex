# Simulate data
n <- 100
p <- 2
beta <- c(1, 1)
beta0 <- 0
x <- matrix(rnorm(n = n * p), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
lp <- x %*% beta + beta0 + 2*x[, 1] * x[, 2]
y <- lp + rnorm(n)

x_train <- x[1:50, ]
x_test <- x[51:100, ]
y_train <- y[1:50]
y_test <- y[51:100]

ybin <- ifelse(1 / (1 + exp(-lp)) > .5, 1, 0)
ybin_train <- ybin[1:50]
ybin_test <- ybin[51:100]

# xgboost: regression
dtrain <- xgboost::xgb.DMatrix(data = x_train, label = y_train)
xg <- xgboost::xgb.train(
  params = list(objective = "reg:squarederror", max_depth = 4, eta = .1),
  data = dtrain,
  nrounds = 10,
  verbose = 0
)
pred_train <- predict(xg, x_train)
pred_test <- predict(xg, x_test)

# Decompositions
res_train <- glex(xg, x_train)
res_test <- glex(xg, x_test)

# xgboost: binary classif
xgbin <- xgboost::xgb.train(
  params = list(objective = "binary:logistic", max_depth = 4, eta = .1),
  data = xgboost::xgb.DMatrix(data = x_train, label = ybin_train),
  nrounds = 10,
  verbose = 0
)
predbin_train <- predict(xgbin, x_train)
predbin_test <- predict(xgbin, x_test)
predbin_margin_train <- predict(xgbin, x_train, outputmargin = TRUE)
predbin_margin_test <- predict(xgbin, x_test, outputmargin = TRUE)

# Decompositions
resbin_train <- glex(xgbin, x_train)
resbin_test <- glex(xgbin, x_test)

test_that("regr: Prediction is approx. same as sum of shap + intercept, training data", {
  expect_equal(unname(res_train$intercept + rowSums(res_train$shap)),
               unname(pred_train),
               tolerance = 1e-5)
})

test_that("binary: Margin is approx. same as sum of shap + intercept, training data", {
  expect_equal(unname(resbin_train$intercept + rowSums(resbin_train$shap)),
               unname(predbin_margin_train),
               tolerance = 1e-5)
})

test_that("regr: Prediction is approx. same as sum of shap + intercept, test data", {
  expect_equal(unname(res_test$intercept + rowSums(res_test$shap)),
               unname(pred_test),
               tolerance = 1e-5)
})

test_that("binary: Margin is approx. same as sum of shap + intercept, test data", {
  expect_equal(unname(resbin_test$intercept + rowSums(resbin_test$shap)),
               unname(predbin_margin_test),
               tolerance = 1e-5)
})

test_that("regr: Prediction is approx. same as sum of decomposition + intercept, training data", {
  expect_equal(unname(res_train$intercept + rowSums(res_train$m)),
               unname(pred_train),
               tolerance = 1e-5)
})

test_that("classif: Margin is approx. same as sum of decomposition + intercept, training data", {
  expect_equal(unname(resbin_train$intercept + rowSums(resbin_train$m)),
               unname(predbin_margin_train),
               tolerance = 1e-5)
})

test_that("regr: Prediction is approx. same as sum of decomposition + intercept, test data", {
  expect_equal(unname(res_test$intercept + rowSums(res_test$m)),
               unname(pred_test),
               tolerance = 1e-5)
})

test_that("binary: Margin is approx. same as sum of decomposition + intercept, test data", {
  expect_equal(unname(resbin_test$intercept + rowSums(resbin_test$m)),
               unname(predbin_margin_test),
               tolerance = 1e-5)
})

test_that("binary: Probability is approx. same as plogis(sum of shap + intercept), training data", {
  expect_equal(
    unname(plogis(resbin_train$intercept + rowSums(resbin_train$shap))),
    unname(predbin_train),
    tolerance = 1e-5
  )
})

test_that("binary: Probability is approx. same as plogis(sum of shap + intercept), test data", {
  expect_equal(
    unname(plogis(resbin_test$intercept + rowSums(resbin_test$shap))),
    unname(predbin_test),
    tolerance = 1e-5
  )
})

test_that("Shap is computed correctly with overlapping colnames", {
  n <- 100
  p <- 2
  beta <- c(1, 1)
  beta0 <- 0
  x <- matrix(rnorm(n = n * p), ncol = p,
              dimnames = list(NULL, paste0('x', seq_len(p))))

  lp <- x %*% beta + beta0 + 2*x[, 1] * x[, 2]
  y <- lp + rnorm(n)

  # xgboost
  set.seed(5)
  xg1 <- xgboost::xgb.train(
    params = list(objective = "reg:squarederror", max_depth = 4, eta = .1),
    data = xgboost::xgb.DMatrix(data = x, label = y),
    nrounds = 10,
    verbose = 0
  )

  # Decompositions
  # x names are unique: x1, x2
  unique_names <- glex(xg1, x)$shap

  # make names overlapping such that grep("temp") also matches "atemp"
  colnames(x) <- c("temp", "atemp")

  # re-train xgboost because changing colnames of x and running glex on that crashes R
  set.seed(5)
  xg2 <- xgboost::xgb.train(
    params = list(objective = "reg:squarederror", max_depth = 4, eta = .1),
    data = xgboost::xgb.DMatrix(data = x, label = y),
    nrounds = 10,
    verbose = 0
  )

  overlapping_names <- glex(xg2, x)$shap

  # We only check values for equality, colnames will of course differ
  expect_equal(unname(overlapping_names), unname(unique_names))

})
