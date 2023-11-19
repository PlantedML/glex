#' Bikesharing data
#'
#' A reduced version of the `Bikeshare` data as included with `ISLR2`.
#' The dataset has been converted to a [`data.table`], with the following changes:
#'
#' - `hr` has been copnverted to a numeric
#' - `workingday` was recoded to a binary `factor` with labels `c("No Workingday", "Workingday")`
#' - `season` was recoded to a `factor` with labels `c("Winter", "Spring", "Summer", "Fall")`
#' - Variables `atemp`, `day`, `registered` and `casual` were removed
#'
#' @source `Bikeshare` in package `ISLR2`
"bike"
