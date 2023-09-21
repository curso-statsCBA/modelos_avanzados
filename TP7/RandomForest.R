library(randomForest)
attach(Boston)

# Divido en datos de train y test
train <- sample(1:nrow(Boston), nrow(Boston)/2)
boston.test <-Boston[-train,"medv"]

# Pruebo un Random Forest
set.seed(1)
bag.boston <- randomForest(medv~., data = Boston,
      subset = train, mtry = 8, importance = TRUE)
bag.boston

# Como predice en test en Random Forest
yhat.bag <- predict(bag.boston,newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag - boston.test)^2)

#Podemos cambiar el n+umero de árboles usando el argumento mtry
set.seed (1)
rf.boston<-randomForest(medv~., data = Boston ,
          subset = train , mtry = 6, importance = TRUE )
yhat.rf <- predict(rf.boston, newdata = Boston [-train , ])
mean((yhat.rf- boston.test)^2)

#Vemos la importancia de cada variable
importance(rf.boston)

#Gráfico
varImpPlot(rf.boston)






