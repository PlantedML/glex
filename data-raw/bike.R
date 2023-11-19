## code to prepare `bike` dataset goes here
library(data.table)
if (!("ISLR2" %in% installed.packages())) {
  install.packages("ISLR2")
}

data("Bikeshare", package = "ISLR2")

bike <- data.table(Bikeshare)
bike[, hr := as.numeric(as.character(hr))]
bike[, workingday := factor(workingday, levels = c(0, 1), labels = c("No Workingday", "Workingday"))]
bike[, season := factor(season, levels = 1:4, labels = c("Winter", "Spring", "Summer", "Fall"))]
bike[, atemp := NULL]
bike[, day := NULL]
bike[, registered := NULL]
bike[, casual := NULL]

usethis::use_data(bike, overwrite = TRUE)
