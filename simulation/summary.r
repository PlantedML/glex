# target parameter A
get_true_shap_SE <- function(glex_obj, true_shap_fun) {
  x <- glex_obj$x
  shap_SEs <- c()
  
  for (d in 1:ncol(x)) {
    true_shap_val <- true_shap_fun(x, d)
    glex_shap <- glex_obj$shap[, ..d]
    SE <- (true_shap_val - glex_shap)^2

    shap_SEs <- c(shap_SEs, SE)
  }

  shap_SEs
}

get_true_shap_MSE <- function(glex_obj, true_shap_fun) {
  SEs <- get_true_shap_SE(glex_obj, true_shap_fun)
  lapply(SEs, mean)
}


# target parameter B
get_emp_shap_MSE <- function(glex_obj, glex_target_b) {
  lapply((glex_target_b$shap - glex_obj$shap)^2, mean)
}

# m_target is data.table of "x1", "x2" and "x1:x2"
get_components_MSE <- function(glex_obj, m_target) {
  lapply((glex_obj$m - m_target)^2, mean)
}
