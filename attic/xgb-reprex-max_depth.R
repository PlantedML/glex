library(xgboost)
library(glex)
library(data.table)
set.seed(3)
x <- as.matrix(mtcars[, -1])

res <- rbindlist(lapply(seq_len(ncol(x)), function(max_depth) {
  xg <- xgboost(
    x,
    mtcars$mpg,
    nrounds = 50,
    verbose = 0,
    params = list(max_depth = max_depth)
  )
  glexb <- glex(xg, x)

  # remove terms with very small values
  idx <- which(sapply(glexb$m, function(term) {
    !all(abs(term) <= 1e-10)
  }))
  glexb$m <- glexb$m[, idx, with = FALSE]

  print(waldo::compare(
    rowSums(glexb$m),
    rowSums(glexb$shap),
    tolerance = 1e-15
  ))

  # Names of components as per glex. terms separated by : for interactions
  terms <- names(glexb$m)
  degrees <- stringr::str_count(terms, ":") + 1
  max_degrees <- max(degrees)

  data.frame(
    max_depth = max_depth,
    max_degree_term = max_degrees,
    diff = max_degrees - max_depth
  )
}))

res

library(ggplot2)
ggplot(res, aes(x = max_depth, y = max_degree_term)) +
  geom_abline() +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = scales::pretty_breaks())


xg <- xgboost(x, mtcars$mpg, nrounds = 50, verbose = 0)
glexb <- glex(xg, x, max_interaction = 4)
terms <- names(glexb$m)
degrees <- stringr::str_count(terms, ":") + 1
max(degrees)

xg$params

trees <- xgboost::xgb.model.dt.tree(model = xg, use_int_id = TRUE)

trees[, list(nodes = .N), by = "Tree"]
