rm(list=ls())
source("./AlternateLinearModel.R")

# data preparation
data("Boston", package = "MASS")
str(Boston)

boston.sc <- scale(Boston)
X <- boston.sc[, -14]
y <- boston.sc[, 14]
# GGally::ggpairs(as.data.frame(boston.sc))


# fit lasso model
require(glmnet)
fit1.cv <- cv.glmnet(X, y)
plot(fit1.cv)
cv.glmnet(X,y)
plot(cv.glmnet(X, y))
lambda.min <- fit1.cv$lambda.min
lambda.1se <- fit1.cv$lambda.1se

str(fit1.cv)
fit1 <- glmnet(X,y)
plot.glmnet(fit1, xvar = "lambda")
abline(v=log(lambda.min), col="red", lty=3)
abline(v=log(lambda.1se), col="blue", lty=3)
abline(v=log(0.07),       col="green", lty=3)


# fit lasso model
require(glmnet)
fit1 <- glmnet(X,y)

# fit Alternate Lasso model
alt1 <- AlternateLasso(X, y, model = fit1, rho = 0.2)

str(alt1)
summary(alt1)

g <- plot(alt1)
g$graph


# compare Alternate score to correlation ----------------------------------
m  <- cor(X)
df <- cbind(g$df, cor=NA)
for(i in 1:NROW(df)){
  df$cor[i] <- m[as.character(df$feature[i]), as.character(df$alt[i])]
}

df
write.csv(df, "cor_boston.csv")
