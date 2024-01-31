source("plot_functions.r")
source("simulation.r")

ubs <- seq(-3, 3, 0.001)
plot_prob_function_diff(1, ubs)
plot_prob_function_diff(2, ubs)

plot_shap(object1, object2, 1)
plot_shap(object1, object2, 2)
plot_shap(object1, object2swapped, 1)
plot_shap(object1, object2swapped, 2)

plot_shap_resid(object1, object2, 1, emp_only = T)
plot_shap_resid(object1, object2, 2, emp_only = T)

plot_components(object1, object2, coords = "x1")
plot_components(object1, object2, coords = "x2")
plot_components(object1, object2, coords = c("x1", "x2"))

plot_components(object1, object2swapped, coords = "x1")
plot_components(object1, object2swapped, coords = "x2")

plot_components_2(object1, object2, coords = "x1")
plot_components_2(object1, object2, coords = "x2")
plot_components_2(object1, object2swapped, coords = "x1")
plot_components_2(object1, object2swapped, coords = "x2")

plot_components_diff(object1, object2, coords = "x1:x2")
plot_components_diff(object1, object2, coords = "x1")
plot_components_diff(object1, object2, coords = "x2")
plot_components_diff(object1, object2swapped, coords = "x1")
plot_components_diff(object1, object2swapped, coords = "x2")
