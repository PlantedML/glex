devtools::load_all()
source("simulate_functions.r")
source("plot_functions.r")
library(mlr3verse)

dataset <- simulate_dat(mu, sigma, n = 1e4)

cv_and_obtain_learner <- function(
  dataset,
  nrounds = to_tune(1, 1000),
  eta = to_tune(0.01, 0.3),
  max_depth = to_tune(2, 6),
  n_evals = 50,
  folds = 5
) {
  df <- data.frame(dataset$x, y = dataset$y)

  # Define the Task
  task <- as_task_regr(df, id = "my_task", target = "y")

  # Initialize the Learner
  learner <- lrn("regr.xgboost",
    nrounds = nrounds,
    eta = eta,
    max_depth = max_depth,
    objective = "reg:squarederror"
  )

  # Create a Tuner
  tuner <- tnr("random_search")

  # Tune the Model
  instance <- ti(
    task = task,
    learner = learner,
    resampling = rsmp("cv", folds = folds),
    measures = msr("regr.mse"),
    terminator = trm("evals", n_evals = n_evals)
  )

  tuner$optimize(instance)

  learner$param_set$values <- instance$result_learner_param_vals
  learner$train(task)

  list(learner = learner, instance = instance, task = task)
}

res <- cv_and_obtain_learner(dataset)
learner <- res[[1]]
instance <- res[[2]]

autoplot(instance, type = "pairs")
autoplot(task, type = "pairs")

xg <- learner$model
object1 <- glex(xg, dataset$x, probFunction = probFunction)
object2 <- glex(xg, dataset$x, probFunction = probFunctionEmp)

# plots
plot_shap(object1, object2, 1)
plot_shap(object1, object2, 2)

plot_shap_resid(object1, object2, 1, emp_only = T)
plot_shap_resid(object1, object2, 2, emp_only = T)

plot_components(object1, object2, coords = "x1")
plot_components(object1, object2, coords = "x2")
plot_components(object1, object2, coords = c("x1", "x2"))
