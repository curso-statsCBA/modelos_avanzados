library(ISLR2)
library(car)
library(MASS)
library(GGally)

data(Boston)
?Boston

g1 <- ggpairs(Boston, aes(alpha = 0.1)) + theme_bw()
g1

fit <- lm(medv ~ ., data = Boston)
vif(fit)

fit2 <- lm(medv ~ . - tax, data = Boston)
vif(fit2)

layout(matrix(1:4,2,2))
plot(fit2)
layout(1)

summary(fit2)
Anova(fit2, type = "II")

stepAIC(fit2)
