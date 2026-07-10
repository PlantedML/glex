library(xgboost)
library(glex)

n <- 500000
n <- 500
set.seed(42)
x1 <- sample(0:1, n, replace = TRUE) ## female/male
x2 <- sample(18:65, n, replace = TRUE) ## age
x3 <- sample(0:1, n, replace = TRUE) # split no/yes
x4 <- sample(0:1, n, replace = TRUE) # sports car no/yes

x <- cbind(x1, x2, x3, x4)

getlambda <- function(x1, x2, x3, x4) {
  0.1 *
    (1 + 0.1 * (x1 == 1)) *
    (1 + (1 / (sqrt(x2 - 17)))) *
    (1 +
      (0.3 * (18 <= x2) * (x2 <= 35)) * (x4 == 1) -
      (0.3 * (45 <= x2) * (x2 <= 65)) * (x4 == 1))
}

x <- cbind(x1, x2, x3, x4)
y <- sapply(1:n, function(i) rpois(1, getlambda(x1[i], x2[i], x3[i], x4[i])))


xg <- xgboost(
  data = x,
  label = y,
  params = list(max_depth = 4, eta = .01, objective = "count:poisson"),
  nrounds = 100,
  verbose = 0
)


trees <- xgboost::xgb.model.dt.tree(model = xg, use_int_id = TRUE)

str(xg)

predict(xg, cbind(x1, x2, x3, x4))


xgdump <- xgb.dump(model = xg, with_stats = TRUE)
xgdump[grepl("leaf=(\\d+)", xgdump)]

xgdump[grepl("leaf=", xgdump)]


xgb.plot.tree(model = xg)

generate_data <- function(n, seed = 42) {
  set.seed(seed)
  x1 <- sample(0:1, n, replace = TRUE)
  x2 <- sample(18:65, n, replace = TRUE)
  x3 <- sample(0:1, n, replace = TRUE)
  x4 <- sample(0:1, n, replace = TRUE)

  x <- cbind(x1, x2, x3, x4)

  getlambda <- function(x1, x2, x3, x4) {
    0.1 *
      (1 + 0.1 * (x1 == 1)) *
      (1 + (1 / (sqrt(x2 - 17)))) *
      (1 +
        (0.3 * (18 <= x2) * (x2 <= 35)) * (x4 == 1) -
        (0.3 * (45 <= x2) * (x2 <= 65)) * (x4 == 1))
  }

  x <- cbind(x1, x2, x3, x4)
  y <- sapply(1:n, function(i) rpois(1, getlambda(x1[i], x2[i], x3[i], x4[i])))
  list(x = x, y = y)
}

dat <- generate_data(n = 50000)

xg <- xgboost(
  data = dat$x,
  label = dat$y,
  params = list(max_depth = 4, eta = .01, objective = "count:poisson"),
  nrounds = 100,
  verbose = 0
)

trees <- xgboost::xgb.model.dt.tree(model = xg, use_int_id = TRUE)

xgdump <- xgb.dump(model = xg, with_stats = TRUE)
xgdump[grepl("leaf=(\\d+)", xgdump)]

head(xgdump[grepl("leaf=", xgdump)])
