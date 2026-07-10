library(data.table)

bike <- data.table::copy(glex::bike)

bike[, `:=`(mnth = as.integer(mnth), workingday = as.integer(workingday) - 1)]

# Based on mlr3pipelines PipeOpEncode
# https://github.com/mlr-org/mlr3pipelines/blob/master/R/PipeOpEncode.R
onehot_encode = function(x) {
  # Ensuring we have a data.table, even if it's a matrix for some reason
  x = data.table::as.data.table(x)
  x_names = colnames(x)
  to_encode = x_names[vapply(
    x_names,
    \(x) inherits(x[[x]], c("character", "factor")),
    logical(1)
  )]

  # If no categorical features are found, return input (as DT though)
  if (length(to_encode) == 0) return(x)

  contrast_list = sapply(
    to_encode,
    \(column_name) {
      levels_in = unique(x[[column_name]])

      stats::contr.treatment(levels_in, contrasts = FALSE)
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  cols_encoded = sapply(
    to_encode,
    \(column_name) {
      x = as.character(x[[column_name]])
      current_contrasts = contrast_list[[column_name]]
      current_contrasts[match(x, rownames(current_contrasts)), , drop = FALSE]
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  cols_encoded = data.table::as.data.table(cols_encoded)
  data.table::setnames(
    cols_encoded,
    names(cols_encoded),
    make.names(names(cols_encoded), unique = TRUE)
  )

  # Column rekajiggering would fail if input was single-column
  if (length(x_names) == 1) return(cols_encoded)
  # Bind original data sans recodable variable with newly created ones
  cbind(x[, setdiff(x_names, to_encode), with = FALSE], cols_encoded)
}


bmat = as.matrix(bike)[, "season", drop = FALSE]

as.data.table(bmat) |>
  onehot_encode()

bike_enc = onehot_encode(bike)
setcolorder(
  bike_enc,
  neworder = c("bikers", setdiff(names(bike_enc), "bikers"))
)

bike_xgb = list(
  x = as.matrix(bike_enc[, -1]),
  label = bike_enc$bikers
)

library(xgboost)

xgb <- xgboost(
  data = bike_xgb$x,
  label = bike_xgb$label,
  nrounds = 100,
  verbose = FALSE,
  max_depth = 3
)

xg_glex <- glex::glex(xgb, x = bike_xgb$x)

xg_glex$x$weathersit.clear <- factor(xg_glex$x$weathersit.clear)
xg_glex$x$weathersit.cloudy.misty <- factor(xg_glex$x$weathersit.cloudy.misty)
xg_glex$x$weathersit.light.rain.snow <- factor(
  xg_glex$x$weathersit.light.rain.snow
)
xg_glex$x$weathersit.heavy.rain.snow <- factor(
  xg_glex$x$weathersit.heavy.rain.snow
)

for (col in names(xg_glex$x)) {
  if (identical(sort(unique(xg_glex$x[[col]])), c(0, 1))) {
    xg_glex$x[[col]] <- factor(xg_glex$x[[col]])
  }
}

library(ggplot2)
autoplot(xg_glex, "hr")
autoplot(xg_glex, "holiday")
autoplot(xg_glex, "weathersit.clear")

usethis::use_data(bike_xg, overwrite = TRUE)
