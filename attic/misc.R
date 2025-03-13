library(randomPlantedForest)
library(glex)
library(data.table)
rp <- rpf(mpg ~ cyl + wt + drat, data = mtcars[1:26, ], max_interaction = 3)

glex_rpf <- glex(rp, mtcars[27:32, ])

# All terms
vi_rpf <- glex_vi(glex_rpf)
library(ggplot2)
# Filter to contributions greater 0.1 on the scale of the target
autoplot(vi_rpf, threshold = 0.1)
# Summarize by degree of interaction
autoplot(vi_rpf, by_degree = TRUE)
# Filter by relative contributions greater 0.5%
autoplot(vi_rpf, scale = "relative", threshold = 0.005)


library(xgboost)
library(glex)
xmtcars <- as.matrix(mtcars[, -1])
xgmt <- xgboost(
  xmtcars,
  mtcars$mpg,
  nrounds = 30,
  verbose = 0,
  params = list(max_depth = 2)
)
glexb <- glex(xgmt, xmtcars)

predcontrib <- predict(xgmt, xmtcars, predcontrib = TRUE)
colnames(predcontrib)

head(predcontrib[, 1:3])
head(glexb$shap[, 1:3])

# intercept
predcontrib[, 11]
glexb$intercept

# interaction terms
predint <- predict(xgmt, xmtcars, predinteraction = TRUE)

str(predint)
# interaction terms for first observation, probably?
head(predint[1, , ])

dimnames(predint)

# main term cyl
predint[, 1, 1]
glexb$m$cyl

# cyl:disp
predint[, 1, 2]
glexb$m$`cyl:disp` / 2 # divide by 2 for SHAP decomposition thing probably?


library(xgboost)
library(glex)
xmtcars <- as.matrix(mtcars[, -1])
xgmt <- xgboost(
  xmtcars,
  mtcars$mpg,
  nrounds = 30,
  verbose = 0,
  params = list(max_depth = 2)
)

bench::mark(
  glex = glex(xgmt, xmtcars),
  xgb = {
    # shap
    predict(xgmt, xmtcars, predcontrib = TRUE)
    # two-way interactions + main terms
    predict(xgmt, xmtcars, predinteraction = TRUE)
  },
  check = FALSE
) |>
  plot()

# Binary classif --------------------------------------------------------------------------------------------------

xdat <- data.frame(
  x1 = rnorm(100),
  x2 = rpois(100, 2),
  x3 = runif(100)
)

xdat$lp = 3 * xdat$x1 + 0.5 * (xdat$x2 + xdat$x3) + 3 * abs(xdat$x1 * xdat$x3)
xdat$p = 1 / (1 + exp(-xdat$lp))
xdat$y = factor(
  rbinom(100, size = 1, prob = xdat$p),
  labels = c("Negative", "Positive")
)
xdat$yk = factor(
  rbinom(100, size = 2, prob = xdat$p),
  labels = c("N", "P", "K")
)


rpb <- rpf(y ~ x1 + x2 + x3, data = xdat, max_interaction = 3)

glex_b <- glex(rpb, xdatb)


autoplot(glex_b, "x1")
autoplot(glex_b, c("x1", "x2"))
glex_vi(glex_b) |> autoplot()


# Multiclass ------------------------------------------------------------------------------------------------------
library(randomPlantedForest)
penguins <- na.omit(palmerpenguins::penguins)

rpm <- rpf(species ~ ., data = penguins, max_interaction = 3)

glex_m <- glex(rpm, penguins)

autoplot(glex_m, "island")

glex_vi(glex_m) |>
  autoplot(threshold = .01)

glex_explain(glex_m, id = 4, threshold = 0.1)


# multiclass xgb?
library(xgboost)
penguins <- na.omit(palmerpenguins::penguins)
mx <- as.matrix(penguins[, 3:6])
my <- as.numeric(penguins$species) - 1

xgm <- xgboost(
  mx,
  my,
  nrounds = 10,
  verbose = 0,
  objective = "multi:softprob",
  num_class = 3
)
glexgb <- glex(xgm, mx)
predict(xgm, mx, reshape = TRUE)

# binary ----------------------------------------------------------------------------------------------------------

if (!requireNamespace("CASdatasets")) {
  install.packages(
    "CASdatasets",
    repos = "http://cas.uqam.ca/pub/",
    type = "source"
  )
}

data("freMTPLfreq", package = "CASdatasets")
?freMTPLfreq

setDT(freMTPLfreq)
freMTPLfreq[, PolicyID := NULL]

freMTPLfreq[, Claim := ClaimNb > 0]
