old_glex <- glex::glex
devtools::load_all()
source("simulate_functions.r")
source("summary.r")
source("cv.r")



obtain_glex_objs <- function(dataset, model, cov_args) {
  x <- dataset$x
  true_p <- function(...) probFunction_(..., cov_args)
  emp_p <- function(...) probFunctionEmp_(..., x)
  emp_p50 <- function(...) probFunctionEmp_(..., x[1:50, ])
  emp_p100 <- function(...) probFunctionEmp_(..., x[1:100, ])
  emp_p500 <- function(...) probFunctionEmp_(..., x[1:500, ])

  glex_treeshap <- old_glex(model, x)
  glex_true_p <- glex(model, x, probFunction = true_p)
  glex_emp_p <- glex(model, x, probFunction = emp_p)
  glex_emp_p50 <- glex(model, x, probFunction = emp_p50)
  glex_emp_p100 <- glex(model, x, probFunction = emp_p100)
  if (length(x) > 500) glex_emp_p500 <- glex(model, x, probFunction = emp_p500)
  else glex_emp_p500 <- NULL

  list(glex_true_p, glex_treeshap, glex_emp_p, glex_emp_p50, glex_emp_p100, glex_emp_p500)
}

obtain_shap_MSEs <- function(true_shap_fun, glex_true_p, to_explain) {
  emp_shap_MSEs <- lapply(to_explain, function(glex_obj) get_emp_shap_MSE(glex_obj, glex_true_p))
  true_shap_MSEs <- lapply(to_explain, function(glex_obj) get_true_shap_MSE(glex_obj, true_shap_fun))

  emp_shap_MSE_df <- do.call(rbind, lapply(emp_shap_MSEs, unlist))
  true_shap_MSE_df <- do.call(rbind, lapply(true_shap_MSEs, unlist))

  list(A_shap_mse = true_shap_MSE_df, B_shap_mse = emp_shap_MSE_df)
}

obtain_component_MSEs <- function(true_components_fun, glex_true_p, to_explain) {
  true_m <- true_components_fun(glex_true_p$x)
  emp_m <- glex_true_p$m

  true_m_MSEs <- lapply(to_explain, function(glex_obj) get_components_MSE(glex_obj, true_m))
  emp_m_MSEs <- lapply(to_explain, function(glex_obj) get_components_MSE(glex_obj, emp_m))


  true_m_MSEs_df <- do.call(rbind, lapply(true_m_MSEs, unlist))
  emp_m_MSEs_df <- do.call(rbind, lapply(emp_m_MSEs, unlist))

  list(A_m_mse = true_m_MSEs_df, B_m_mse = emp_m_MSEs_df)
}


simulate_inner <- function(n, c, s) {
  sim_dat_res <- simulate_dat_wrapped(n, c, s)

  dataset <- sim_dat_res$dat
  cov_args <- sim_dat_res$cov_args

  res <- cv_and_obtain_learner(dataset)
  model_mse <- res[[2]]$result$regr.mse
  model <- res[[1]]$model

  glex_objs <- obtain_glex_objs(dataset, model, cov_args)
  shap_MSEs <- obtain_shap_MSEs(true_shap_fun = true_shap2, glex_true_p = glex_objs[[1]], to_explain = glex_objs[-1])
  m_MSEs <- obtain_component_MSEs(true_components_fun = true_components_m, glex_true_p = glex_objs[[1]], to_explain = glex_objs[-1])

  stats <- list(shap_MSEs, m_MSEs)
  list(dataset = dataset, model = model, glex_objs = glex_objs, stats = stats)
}


res <- simulate_inner(1e3, 0.3, F)
