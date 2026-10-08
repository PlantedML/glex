make_ranger_cat_data <- function(n = 200, seed = 3) {
  set.seed(seed)
  df <- data.frame(
    f = factor(sample(letters[1:5], n, TRUE), levels = letters[1:5]),
    o = factor(sample(c("lo", "mid", "hi"), n, TRUE), levels = c("lo", "mid", "hi"), ordered = TRUE),
    z = rnorm(n)
  )
  df$y <- rnorm(n) + match(df$f, c("c", "a", "e", "b", "d")) * (df$z > 0) + as.integer(df$o)
  df
}

test_that("ranger factor handling decomposes to the prediction in every mode", {
  skip_if_not_installed("ranger")
  df <- make_ranger_cat_data()
  x <- df[c("f", "o", "z")]
  x_new <- make_ranger_cat_data(50, seed = 4)[c("f", "o", "z")]

  for (mode in c("ignore", "order", "partition")) {
    rf <- ranger::ranger(
      y ~ .,
      df,
      num.trees = 10,
      max.depth = 4,
      node.stats = TRUE,
      respect.unordered.factors = mode
    )
    methods <- c("fastpd", "path-dependent", if (mode != "partition") "empirical")
    for (method in methods) {
      for (x_in in list(x, x_new)) {
        gl <- suppressMessages(glex(rf, x_in, weighting_method = method))
        expect_s3_class(gl$x$f, "factor")
        expect_equal(
          unname(gl$intercept + rowSums(gl$m)),
          predict(rf, x_in)$predictions,
          tolerance = 1e-10,
          info = paste(mode, method)
        )
      }
    }
  }
})

test_that("empirical weighting rejects ranger partition splits", {
  skip_if_not_installed("ranger")
  df <- make_ranger_cat_data()
  rf <- ranger::ranger(
    y ~ .,
    df,
    num.trees = 5,
    node.stats = TRUE,
    respect.unordered.factors = "partition"
  )
  expect_error(
    glex(rf, df[c("f", "o", "z")], weighting_method = "empirical"),
    "does not support categorical splits"
  )
})
