library (tree)
library (ISLR2)
attach (Carseats)

?Carseats

##Convertimos la variable en binaria
High <- factor (ifelse(Sales <= 8, "No", " Yes "))

# Agregamos la nueva variable
Carseats <- data.frame(Carseats, High)

# Probamos un árbol
tree.carseats <- tree(High ~ . -Sales, Carseats)
summary(tree.carseats)

# Graficamos el árbol
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.carseats

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
plot(cv.carseats$size, cv.carseats$dev,type = "b")
plot(cv.carseats$k, cv.carseats$dev,type = "b")

# Podamos el árbol y obtenemos un ábol con 9 nodos
prune.carseats <- prune.misclass(tree.carseats, best = 8)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# Cómo funcionan nuestro árbol podado
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred,High.test)

(58+97/200)

