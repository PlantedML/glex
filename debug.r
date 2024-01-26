
devtools::load_all()
library(mvtnorm)
library(xgboost)
library(glex)
library(tidyverse)
library(patchwork)

# Simulate from multivariate normal
n <- 1e5
p <- 2
beta <- c(1, 1)
beta0 <- 0
cov_base <- 0.3
mu <- c(0, 0)
sigma <- toeplitz(cov_base^(0:(p-1)))
x <- matrix(rmvnorm(n = n, mean = mu, sigma = sigma), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
lp <- x %*% beta + beta0 + 2*x[, 1] * x[, 2]
y <- lp + rnorm(n)

# xgboost
xg <- xgboost(data = x, label = y, params = list(max_depth = 4, eta = .1), nrounds = 10, verbose = 0)

# SHAP decomposition
cov_args <- list(mu, sigma)

probFunction <- function(coords, lb, ub) {
    pmvnorm(lower = lb, upper = ub, mean = cov_args[[1]][coords], sigma = cov_args[[2]][coords, coords])
}

probFunctionEmp <- function(coords, lb, ub) {
  mean(apply(t(x[, coords]) > lb & t(x[, coords]) < ub, 2, all))
}




object1 <- glex(xg, x, probFunction = probFunction)
object1$shap

object2 <- glex(xg, x, probFunction = probFunctionEmp)

plotdata <- data.frame(x1=x[,1], x2=x[,2], shapA1=x[,1] + x[,1]*x[,2] - 0.3 , shapA2=x[,2] + x[,1]*x[,2] - 0.3,
                        shapB1=object1$shap$x1, shapB2=object1$shap$x2,
                        shapEmp1=object2$shap$x1, shapEmp2=object2$shap$x2)
plotdata.long <- reshape2::melt(plotdata, id=c("x1","x2"))


theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.border = element_rect(colour = "black", fill=NA, size=0.6),
             text=element_text(family="Helvetica Neue"))
ggplot(data = plotdata.long[grepl( "1", plotdata.long$variable, fixed = TRUE),], aes(x=x1,y=value,color=variable)) +
  geom_point() +
  labs(y="SHAP")

p1=autoplot(object1,c("x1","x2"))
p2=autoplot(object2, c("x1","x2"))
p1=autoplot(object1,"x1")
p2=autoplot(object2, "x1")

p1+p2


mean((x[,1] + x[,1]*x[,2] - 0.3 - object2$shap$x1)**2)
mean((object1$shap$x1 - object2$shap$x1)**2)

residData1 = data.frame(x1=x[,1],
                        resA1 = x[,1] + x[,1]*x[,2] - 0.3 - object2$shap$x1,
                        resB1 = object1$shap$x1 - object2$shap$x1)
residData1.long <- reshape2::melt(residData1, id=c("x1"))


ggplot(data=residData1.long, aes(x=x1,y=value, color=variable)) +
    geom_point()
