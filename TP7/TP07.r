##############
### CASO 1 ###
##############

library(MASS)
library(ggplot2)
library(caret)

attach(iris)
str(iris)

# escalamos cada variable predictora (i.e.  primeras cuatro columnas)
iris[1:4] <- scale(iris[1:4])

# hacemos que el ejemplo sea reproducible
set.seed(1)

# Usamos 70% del dataset como training set y el restante 30% como test set
sample <- sample(c(TRUE, FALSE), nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
train <- iris[sample, ]
test <- iris[!sample, ]

# hacemos el LDA
model <- lda(Species ~ ., data = train)

# vemos  el modelo
model

# usamos el LDA para hacer predicciones
predicted <- predict(model, test)
names(predicted)

# vemos las probilidades posteriores para las primeras 6 observaciones 
# en el test
head(predicted$posterior)

# vemos los discriminantes lineales para las primeras 6 observaciones 
# en el test
head(predicted$x)

# vemos el accuracy de nuestro modelo
mean(predicted$class == test$Species)

# definimos que vamos a graficar
lda_plot <- cbind(train, predict(model)$x)

# creamos el plot
ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = Species))

# Histogramas para los valores de la función discriminante (LDA1)
p <- predict(model, train)
ldahist(data = p$x[, 1], g = train$Species)

# Histogramas para los valores de la función discriminante (LDA2)
ldahist(data = p$x[, 2], g = train$Species)

# Matriz de confusión y Accuracy (en train)
confusionMatrix(p1, train$Species) # con caret

#  de confusión y Accuracy (en testing)
confusionMatrix(p2, test$Species)

##############
### CASO 2 ###
##############

library(tree)
library(ISLR2)
data(Carseats)

?Carseats

# Convertimos la variable en binaria y la agregamos
Carseats$High <- factor(ifelse(Carseats$Sales <= 8, "No", " Yes"))

# Probamos un árbol
tree.carseats <- tree(High ~ . - Sales, Carseats)
summary(tree.carseats)

# Graficamos el árbol
plot(tree.carseats)
text(tree.carseats, pretty = 0)

# Veo en test
set.seed(2)
train <- sample(1: nrow(Carseats), 200)
Carseats.test <- Carseats[-train, ]
High.test<- High[- train]
tree.carseats<- tree(High ~ . -Sales, Carseats, subset = train)
tree.pred<- predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred,High.test)

# Podamos el árbol y vemos si mejora
set.seed(7)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats

# A pesar de su nombre, dev corresponde al número de errores de validación
# cruzada. El árbol con 9 nodos terminales da como resultado sólo 74 errores
# de validación cruzada.

# Ploteamos la tasa de error en función del tamaño y de K
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")
par()

# Podamos el árbol y obtenemos un ábol con 9 nodos
prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# Cómo funcionan nuestro árbol podado
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred,High.test)

##############
### CASO 3 ###
##############

library(randomForest)
attach(Boston)

# Divido en datos de train y test
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
boston.test <- Boston[-train, "medv"]

# Pruebo un Random Forest
set.seed(1)
bag.boston <- randomForest(medv ~ ., data = Boston,
      subset = train, mtry = 8, importance = TRUE)
bag.boston

# Como predice en test en Random Forest
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag - boston.test)^2)

#Podemos cambiar el n+umero de árboles usando el argumento mtry
set.seed(1)
rf.boston<-randomForest(medv ~ ., data = Boston,
          subset = train , mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston [-train, ])
mean((yhat.rf - boston.test)^2)

#Vemos la importancia de cada variable
importance(rf.boston)

#Gráfico
varImpPlot(rf.boston)