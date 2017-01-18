rm(list=ls())
source("./AlternateLinearModel.R")

# data preparation
num = 1000
set.seed(1000)

X <- matrix(0, num, 6)
colnames(X) <-paste0("X_",1:NCOL(X))

X[, 1] <- 0.5 * rnorm(num)
X[, 3] <- 0.7 * X[, 1] + 0.2 * rnorm(num)
X[, 4] <- 0.6 * X[, 1] + 0.2 * rnorm(num)

X[, 2] <- 0.5 * rnorm(num)
X[, 5] <- 0.9 * X[, 2] + 0.2 * rnorm(num)

X[, 6] <- 0.5 * rnorm(num)
GGally::ggpairs(as.data.frame(X))


y <- X[, 1] + 0.5 * X[, 2]  + 0.5 * rnorm(num)



# fit lasso model
require(glmnet)
fit1 <- glmnet(X,y)

# fit Alternate Lasso model
alt1 <- AlternateLasso(X, y, model = fit1, rho = 0.07)

print(alt1)


summary(alt1)


g <- plot(alt1)

g$df

g$graph

