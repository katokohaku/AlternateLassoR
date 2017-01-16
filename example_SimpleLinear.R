rm(list=ls())
source("./AlternateLinearModel.R")
# data preparation
num = 1000
dim = 3

set.seed(1000)
X <- matrix(rnorm(num * (dim + 3)), num, dim + 3)
colnames(X) <-paste0("feature_",1:NCOL(X))
for (i in 1:2){
  X[, dim + i] <- X[, 1] + 0.5 * rnorm(num)
}
for (i in 3){
  X[, dim + i] <- X[, 2] + 0.7 * rnorm(num)
}
y <- X[, 1] + 0.5 * X[, 2]  + 0.5 * rnorm(num)
# GGally::ggpairs(as.data.frame(X))

# fit lasso model
require(glmnet)
fit1 <- glmnet(X,y)

# fit Alternate Lasso model
alt1 <- AlternateLasso(X, y, model = fit1, rho = 0.07)

print(alt1)
g <- plot(alt1)





from <- paste0("very_long_colmn_names_as_feature_", c("A", "A", "A", "B", "B", "B", "B", "C"))
to   <- paste0("very_long_colmn_names_as_feature_", c(1, 1, 2, 3, 1, 4, 1, 2))
plotVerticalBipartiteGraph(left = from, right = to)
