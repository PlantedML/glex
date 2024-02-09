library(patchwork)
source("simulation.r")
theme_set(theme_bw())
theme_update(
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA, size = 0.6)
)


BoxPlot <- function(res, settings, group = n, coord, MSEtype, MSEtypeIsComponents = FALSE) {
  # Plots boxplots of different types of MSE for different settings
  # res: result of simulation
  # settings: settings for which to plot box plot
  # group: variable on x-axis
  # coord: coordinate to show MSE for
  # MSEtype: type of MSE to use
  # MSEtypeIsComponents: set to TRUE if the MSEtype is component MSE
  data <- NULL
  mse_type_listnumber <- ifelse(MSEtypeIsComponents, 2, 1)
  for (i in settings) {
    estType <- c("TreeSHAP", "Emp", "Emp50", "Emp100")
    if(as.numeric(complete_res[[i]]$params[1])>500){
      estType <- c(estType,"Emp500")
    }
    for (j in 1:length(res[[i]]$sim_res)) {
      table <- cbind(res[[i]]$params, res[[i]]$sim_res[[j]]$stats[[mse_type_listnumber]][[MSEtype]], type = estType, row.names = NULL)
      data <- rbind(data, table)
    }
  }
  group_plotvar <- paste0("as.factor(", group, ")")
  p <- ggplot(data, aes_string(x = group_plotvar, y = coord, fill = "type")) +
    geom_boxplot(width = 0.5) +
    labs(x = group, y = MSEtype)
  ggsave(paste0("simulation/boxplot", coord, ".png"), plot = p, width = 10, height = 8, dpi = 300)
  return(p)
}


BoxPlot(res = complete_res, settings = c(2, 3), group = "n", coord = "x1", MSEtype = "A_shap_mse", MSEtypeIsComponents = F)


plotMedianComponents <- function(res, setting, MSEtype, coords, titles, true_function = NULL) {
  # Plots the components of the glex object with median MSE
  # res: result of simulation
  # setting: setting for which to plot
  # MSEtype: type of MSE for which to calculate median MSE. Number in list c("\\hat{m}p", "TreeSHAP", "Emp", "Emp50", "Emp100", "Emp500")
  # coords: coordinates to plot components for
  # tiles: titles of the plots
  # true_function: insert function in plots. Set to NULL if length(coords)>2
  coordsString <- coords[1]
  if (length(coords) > 1) {
    for (i in 2:length(coords)) {
      coordsString <- paste0(coordsString, ":", coords[i])
    }
  }
  glexList <- list(NULL)
  compMSE <- NULL
  for (i in 1:length(res[[setting]]$sim_res)) {
    glexList[[i]] <- res[[setting]]$sim_res[[i]]$glex_objs
    compMSE[i] <- res[[setting]]$sim_res[[i]]$stats[[2]]$B_m_mse[MSEtype, coordsString]
  }
  medianIndex <- which(compMSE == min(compMSE[which(compMSE >= median(compMSE))]))
  p <- list(NULL)
  for (i in 1:length(glexList[[1]])) {
    p[[i]] <- autoplot(glexList[[medianIndex]][[i]], coords) + geom_rug(sides = "b") +
      ggtitle(titles[i]) + stat_function(fun = true_function, col = "red")
  }
  plot <- p[[1]]
  for (i in 2:length(glexList[[1]])) {
    plot <- plot + p[[i]]
  }
  cat("plotting for simulation",medianIndex, "\n")
  ggsave(paste0("simulation/comps", coordsString, ".png"), plot = plot, width = 10, height = 8, dpi = 300)
  return(plot)
}

plotMedianComponents(
  res = complete_res, setting = 3, MSEtype = 3, coords = c("x1"),
  titles = c("\\hat{m}p", "TreeSHAP", "Emp", "Emp50", "Emp100", "Emp500"),
  true_function = function(x) {x - 2 * 0.3}
)

plotComponentIters = function(res, setting, coord, titles, true_function = NULL){
  # Plots the components of the glex object for all simulations together
  # res: result of simulation
  # setting: setting for which to plot
  # coord: coordinate to plot components for
  # tiles: titles of the plots
  # true_function: insert function in plots. Set to NULL if length(coords)>2
  p = list(NULL)
  for(i in 1:length(res[[setting]]$sim_res[[1]]$glex_objs)){
    data = NULL
    cat("calculating components for",c("\\hat{m}p", "TreeSHAP", "Emp", "Emp50", "Emp100", "Emp500")[i],"\n")
    for(j in 1:100){
      data = rbind(data, data.frame(x = res[[setting]]$sim_res[[j]]$glex_objs[[i]]$x[[coord]],
                                    m = res[[setting]]$sim_res[[j]]$glex_objs[[i]]$m[[coord]],
                                    It = as.factor(j)))
    }
    p[[i]] = ggplot(data, aes(x=x,y=m)) +
              geom_line(aes(group=It), alpha = 0.05) +
              geom_rug(sides="b") +
              labs(x = coord, y = paste0("\\hat{m}",substring(coord, 2, 2)), title = titles[i]) +
              stat_function(fun = true_function, col = "red")
  }
  cat("plotting...","\n")
  plot <- p[[1]]
  for (i in 2:length(p)){
    plot <- plot + p[[i]]
  }
  ggsave(paste0("simulation/compItersS", setting, coord, ".png"), plot = plot, width = 10, height = 8, dpi = 300)
  return(plot)
}

plotComponentIters(res = complete_res, setting = 1, coord = "x1",
  titles = c("\\hat{m}p", "TreeSHAP", "Emp", "Emp50", "Emp100", "Emp500"),
  true_function = function(x) {x})


plotMedianShap <- function(res, setting, MSEtype, coord, titles) {
  # Plots the shap of the glex object with median MSE
  # res: result of simulation
  # setting: setting for which to plot
  # MSEtype: type of MSE for which to calculate median MSE. Number in list c("\\hat{m}p", "TreeSHAP", "Emp", "Emp50", "Emp100", "Emp500")
  # coord: coordinate to plot shap values for
  # tiles: titles of the plots
  shapMSE <- NULL
  for (i in 1:length(res[[setting]]$sim_res)) {
    shapMSE[i] <- res[[setting]]$sim_res[[i]]$stats[[1]]$B_shap_mse[MSEtype, coord]
  }
  medianIndex <- which(shapMSE == min(shapMSE[which(shapMSE >= median(shapMSE))]))
  p <- list(NULL)
  x <- res[[setting]]$sim_res[[medianIndex]]$dataset$x
  data <- data.frame(x,
    shapA = x[, 1] + x[, 1] * x[, 2] - 0.3,
    shapB = res[[setting]]$sim_res[[medianIndex]]$glex_objs[[1]]$shap[[coord]]
  )
  plot <- ggplot(reshape2::melt(data, id = colnames(x)), aes_string(x = coord, y = "value", color = "variable")) +
    geom_point(alpha = 0.1) +
    scale_color_manual(values = c("red", "blue")) +
    labs(y = "shap", title = titles[1]) +
    theme(legend.position = "none")
  for (i in 2:length(res[[setting]]$sim_res[[medianIndex]]$glex_objs)) {
    plotdata <- data.frame(data, shapVar = res[[setting]]$sim_res[[medianIndex]]$glex_objs[[i]]$shap[[coord]])
    colnames(plotdata)[length(plotdata)] <- "See title" # titles[i]
    plotdata <- reshape2::melt(plotdata, id = colnames(x))
    p[[i]] <- ggplot(plotdata, aes_string(x = coord, y = "value", color = "variable")) +
      geom_point(alpha = 0.1) +
      scale_color_manual(values = c("red", "blue", "green")) +
      labs(y = "shap", title = titles[i], color = "SHAP type")
    plot <- plot + p[[i]]
  }
  plot <- plot + plot_layout(guides = "collect")
  ggsave(paste0("simulation/shaps", coord, ".png"), plot = plot, width = 10, height = 8, dpi = 300)
  return(plot)
}


plotMedianShap(res = complete_res, setting = 2, MSEtype = 3, coord = "x1", titles = c("Reference", "TreeSHAP", "Emp", "Emp50", "Emp100", "Emp500"))


# load("Complete.Rdata")
# for(i in 1:3){
# names(complete_res[[i]]) = c("params","sim_res")
#   complete_res[[i]]$params = data.frame(n=complete_res[[i]]$params[1], c = complete_res[[i]]$params[2], s = complete_res[[i]]$params[3])
#   rownames(complete_res[[i]]$params) = NULL
# }

