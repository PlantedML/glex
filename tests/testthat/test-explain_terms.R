test_that("terms run from the intercept to the prediction", {
  gl <- fake_glex()
  et <- explain_terms(gl, id = 2)
  expect_equal(et$intercept, 19.56)
  expect_equal(et$prediction, 19.56 + sum(unlist(gl$m[2, ])))
  expect_equal(et$terms$start[1], et$intercept)
  expect_equal(et$terms$end[nrow(et$terms)], et$prediction)
  expect_equal(et$terms$start[-1], et$terms$end[-nrow(et$terms)])
  expect_identical(et$terms$term[1:2], c("hp", "cyl"))
  expect_identical(et$n_other, 0L)
})

test_that("zero components are not counted as terms", {
  et <- explain_terms(fake_glex(), id = 1)
  expect_false("cyl:wt" %in% et$terms$term)
  expect_identical(nrow(et$terms), 6L)
})

test_that("max_terms keeps the largest terms and aggregates the rest", {
  et <- explain_terms(fake_glex(), id = 2, max_terms = 2)
  expect_identical(et$terms$term, c("hp", "cyl", "5 other terms"))
  expect_identical(et$terms$type, c("term", "term", "other"))
  expect_equal(et$terms$m[3], 0.21 + 0.08 - 0.14 + 0.04 - 0.03)
  expect_identical(et$n_other, 5L)
})

test_that("threshold and max_interaction aggregate terms", {
  gl <- fake_glex()
  et <- explain_terms(gl, id = 2, threshold = 0.1)
  expect_identical(et$terms$term, c("hp", "cyl", "wt", "cyl:hp", "3 other terms"))
  et <- explain_terms(gl, id = 2, max_interaction = 1)
  expect_identical(et$terms$term, c("hp", "cyl", "wt", "4 other terms"))
  expect_equal(et$terms$m[4], 0.08 - 0.14 + 0.04 - 0.03)
  et <- explain_terms(gl, id = 2, threshold = 0.1, max_interaction = 1, max_terms = 1)
  expect_identical(et$terms$term, c("hp", "6 other terms"))
})

test_that("edge cases of the selection keep the sum exact", {
  gl <- fake_glex()
  full <- explain_terms(gl, id = 2)
  expect_identical(explain_terms(gl, id = 2, max_terms = 100)$terms$term, full$terms$term)
  et <- explain_terms(gl, id = 2, threshold = 10)
  expect_identical(et$terms$term, "7 other terms")
  expect_equal(et$prediction, full$prediction)
  expect_identical(explain_terms(gl, id = 2, max_terms = 6)$terms$term[7], "1 other term")

  gl$m[3, (names(gl$m)) := 0]
  et <- explain_terms(gl, id = 3)
  expect_identical(nrow(et$terms), 0L)
  expect_equal(et$prediction, et$intercept)
})

test_that("labels show the observation's feature values", {
  et <- explain_terms(fake_glex(), id = 2)
  expect_identical(et$terms$label[et$terms$term == "hp"], "hp = 264")
  expect_identical(et$terms$label[et$terms$term == "cyl:hp"], "cyl:hp = 8, 264")
  et <- explain_terms(fake_glex(), id = 2, max_terms = 2)
  expect_identical(et$terms$label[3], "5 other terms")
})

test_that("labels tolerate features missing from or NA in $x", {
  x_row <- data.table::data.table(hp = 264, engine = NA_character_)
  expect_identical(
    term_label(c("engine", "engine:hp", "wt"), x_row),
    c("engine = NA", "engine:hp = NA, 264", "wt = NA")
  )
})

test_that("multiclass objects need a class and use its terms", {
  gl <- fake_glex_multiclass()
  expect_error(explain_terms(gl, 1), "`class` is required.*a, b")
  expect_error(explain_terms(gl, 1, class = "c"))
  expect_error(explain_terms(fake_glex(), 1, class = "a"), "only applies to multiclass")

  et <- explain_terms(gl, 1, class = "b")
  expect_setequal(et$terms$term, c("hp", "wt", "hp:wt"))
  expect_equal(et$prediction, 0.5 - 0.4 + 0.1 - 0.05)

  gl$intercept <- c(0.2, 0.7)
  expect_equal(explain_terms(gl, 1, class = "b")$intercept, 0.7)
})

test_that("constrained objects end with the remainder", {
  gl <- fake_glex()
  gl$remainder <- c(0.5, -0.25, 0.1)
  gl$constrained <- "max_interaction"
  et <- explain_terms(gl, id = 2)
  last <- et$terms[nrow(et$terms), ]
  expect_identical(last$type, "remainder")
  expect_identical(last$term, "remainder (constrained)")
  expect_equal(et$prediction, 19.56 + sum(unlist(gl$m[2, ])) - 0.25)

  glm <- fake_glex_multiclass()
  glm$remainder <- data.table::data.table(a = c(1, 2), b = c(3, 4))
  expect_equal(explain_terms(glm, 2, class = "b")$terms$m[4], 4)
})

test_that("explain_terms reaches the model prediction for real models", {
  x <- as.matrix(mtcars[, -1])
  xg <- xgboost::xgb.train(
    params = list(max_depth = 3, nthread = 1),
    data = xgboost::xgb.DMatrix(x, label = mtcars$mpg),
    nrounds = 10
  )
  gx <- suppressMessages(glex(xg, x))
  expect_equal(
    explain_terms(gx, 4, max_terms = 3)$prediction,
    unname(predict(xg, x)[4]),
    tolerance = 1e-5
  )

  skip_if_not_installed("ranger")
  set.seed(1)
  d <- transform(mtcars, cyl = factor(cyl))
  rf <- ranger::ranger(mpg ~ ., data = d, num.trees = 5, node.stats = TRUE)
  gr <- suppressMessages(glex(rf, d[, -1]))
  expect_equal(
    explain_terms(gr, 4, max_terms = 3)$prediction,
    predict(rf, d[, -1])$predictions[4],
    tolerance = 1e-10
  )

  skip_if_not_installed("randomPlantedForest", minimum_version = "0.5.0.9000")
  set.seed(1)
  rp <- rpf(mpg ~ ., data = mtcars, max_interaction = 2)
  target <- predict(rp, mtcars[4, ])$.pred
  expect_equal(explain_terms(glex(rp, mtcars), 4, max_terms = 3)$prediction, target, tolerance = 1e-8)
  gc <- suppressWarnings(glex(rp, mtcars, max_interaction = 1))
  expect_equal(explain_terms(gc, 4)$prediction, target, tolerance = 1e-8)
})

test_that("class_intercept is vectorized over classes", {
  gl <- fake_glex_multiclass()
  gl$intercept <- c(a = 0.2, b = 0.7)
  expect_equal(class_intercept(gl, c("b", "a", "b")), c(0.7, 0.2, 0.7))
  gl$intercept <- 0.5
  expect_equal(class_intercept(gl, c("b", "a")), 0.5)
})

test_that("missing components give an informative error", {
  gl <- fake_glex()
  gl$m[2, hp := NA]
  expect_error(explain_terms(gl, id = 2), "`object\\$m` contains missing values for observation 2")
})
