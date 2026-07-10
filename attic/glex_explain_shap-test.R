library(ggplot2)
library(data.table)
library(randomPlantedForest)

mtcars$vs <- factor(
  mtcars$vs,
  levels = c(0, 1),
  labels = c("V-Shaped", "Straight")
)
rp <- rpf(mpg ~ drat + wt + hp + carb + vs, data = mtcars, max_interaction = 2)

glex_rpf <- glex(rp, mtcars)

glex_explain(glex_rpf, 18, threshold = 0.001, predictors = c("hp", "vs", "wt"))


data(Bikeshare, package = "ISLR2")
bike <- data.table(Bikeshare)
bike[, hr := as.numeric(as.character(hr))]
bike[,
  workingday := factor(
    workingday,
    levels = c(0, 1),
    labels = c("No Workingday", "Workingday")
  )
]
bike[,
  season := factor(
    season,
    levels = 1:4,
    labels = c("Winter", "Spring", "Summer", "Fall")
  )
]

# Only one observation with this condition, removing it to make space.
bike <- bike[weathersit != "heavy rain/snow", ]
rp <- rpf(
  bikers ~ day + hr + temp + windspeed + workingday + weathersit,
  data = bike,
  max_interaction = 3,
  ntrees = 30
)

purify(rp)
vars <- c("hr", "temp", "workingday", "weathersit")

components <- glex(rp, bike, predictors = vars)

glex_explain(
  components,
  130,
  max_interaction = 2,
  predictors = c("hr", "temp", "workingday"),
  barheight = 0.5
)
glex_explain(components, 130, threshold = 0.5, predictors = "hr")
glex_explain(
  components,
  130,
  max_interaction = 2,
  predictors = "workingday",
  barheight = 0.5
)

glex_explain(components, 130, max_interaction = 1, predictors = "hr")


# multiclass
penguins <- na.omit(palmerpenguins::penguins)
rppeng <- rpf(species ~ ., data = penguins, max_interaction = 3, ntrees = 30)
glex_peng <- glex(rppeng, penguins)

glex_explain(
  glex_peng,
  3,
  max_interaction = 1,
  predictors = "bill_length_mm",
  class = "Adelie"
)
