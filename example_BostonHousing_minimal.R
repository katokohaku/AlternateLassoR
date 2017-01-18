rm(list=ls())

source("./AlternateLinearModel.R")

# data preparation
data("Boston", package = "MASS")
boston.sc <- scale(Boston)
X <- boston.sc[, -14]
y <- boston.sc[, 14]

# estimate lambda of lasso using cross-validation
require(glmnet)
fit1.cv <- cv.glmnet(X, y)
fit1    <- glmnet(X,y)
lambda  <- fit1.cv$lambda.1se
coef(fit1, s = lambda)

# fit Alternate Lasso model
alt1 <- AlternateLasso(X, y, model = fit1, rho = lambda)
summary(alt1)

g <- plot(alt1)
g$graph
g$df

