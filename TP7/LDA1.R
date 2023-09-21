library(MASS)
library(ggplot2)
library(caret)

attach(iris)
str(iris)

# escalamos cada variable predictora (i.e.  primeras cuatro columnas)
iris[1:4] <- scale(iris[1:4])

# hacemos que el ejemlo sea reproducible
set.seed(1)

# Usamos 70% del dataset como training set y el estante 30% como testing set
sample <- sample(c(TRUE, FALSE), nrow(iris), replace=TRUE, prob=c(0.7,0.3))
train <- iris[sample, ]
test <- iris[!sample, ] 

# hacemos el LDA
model <- lda(Species~., data = train)

# vemos  el modelo 
model

# usamos el LDA para hacer predicciones
predicted <- predict(model, test)
names(predicted)

# view lo que predijo el modelo para las primeras 6 observaciones en test
head(predicted$class)

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
ldahist(data = p$x[,1], g = train$Species)

# Histogramas para el LDA2
ldahist(data = p$x[,2], g = train$Species)

# Matriz de confusión y Accuracy (en train)
p1 <- predict(model, train)$class
tab <- table(Predicted = p1, Actual = train$Species)
tab
sum(diag(tab))/sum(tab)

confusionMatrix(p1, train$Species) # con caret

#  de confusión y Accuracy (en testing)
p2 <- predict(model, test)$class
tab1 <- table(Predicted = p2, Actual = test$Species)
tab1
sum(diag(tab1))/sum(tab1)

confusionMatrix(p2, test$Species)



