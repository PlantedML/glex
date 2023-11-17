set.seed(234)

# Simulated data with binary/multiclass target
xdat <- data.frame(
  x1 = rnorm(100),
  x2 = rpois(100, 2),
  x3 = runif(100)
)


# this is completely arbitrary nonsense.
xdat$x4 <- as.factor(as.integer(xdat$x1 > .2))
xdat$x5 <- as.factor(ceiling(xdat$x2 / 3))
xdat$x6 <- as.factor(ceiling(xdat$x1 / 3))
xdat$lp = 3 * xdat$x1 + 0.5 * (xdat$x2 + xdat$x3) + 3 * abs(xdat$x1 * xdat$x3) + as.integer(xdat$x4) * as.integer(xdat$x5)
xdat$p = 1/(1 + exp(-xdat$lp))
xdat$y = factor(rbinom(100, size = 1, prob = xdat$p), labels = c("Negative", "Positive"))
xdat$yk = factor(rbinom(100, size = 2, prob = xdat$p), labels = c("N", "P", "K"))
