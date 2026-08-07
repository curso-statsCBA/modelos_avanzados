library(mclust)
library(vegan)

PCscores <- read.table("TP9/pc_scores.txt", header = TRUE)
xyscores <- read.table("TP9/coordenadas_planta.txt", header = TRUE)
DAT <- data.frame(PCscores, xyscores[, 4:31])

###############################
# gaussian admixture analysis #
###############################

variables <- c("PC1", "PC2")
BIC <- mclustBIC(DAT[, variables])
plot(BIC)
summary(BIC)

# best model
mod1 <- Mclust(DAT[, variables], x = BIC)
summary(mod1, parameters = TRUE)

# second model
mod2 <- Mclust(DAT[, variables], G = 3, modelNames = "EVV")
summary(mod2, parameters = TRUE)

# third model
mod3 <- Mclust(DAT[, variables], G = 4, modelNames = "EVE")
summary(mod3, parameters = TRUE)

plot(mod1, what = "classification")

plot(mod2, what = "classification")

plot(mod3, what = "classification")

#comparación entre la clasificación a priori y con GMA
table(DAT$class, mod1$classification)

colores <- as.character(DAT$class)
colores[colores == "Pen"] <- "#e35e06"
colores[colores == "Lax"] <- "#19cf37"
colores[colores == "Hib"] <- "#0258af"

plot(DAT[, c("PC1", "PC2")], type = "n")
ordihull(
  DAT[, c("PC1", "PC2")], groups = mod1$classification,
  draw = "polygon", col = "grey", lwd = 0.1
)
points(DAT[, c("PC1", "PC2")], col = colores, pch = 19)

############
## DBSCAN ##
############

library(fpc)

Dbscan_cl1 <- dbscan(DAT[, variables], eps = 0.1, MinPts = 5)
Dbscan_cl2 <- dbscan(DAT[, variables], eps = 0.1, MinPts = 10)
Dbscan_cl3 <- dbscan(DAT[, variables], eps = 0.05, MinPts = 5)
Dbscan_cl4 <- dbscan(DAT[, variables], eps = 0.05, MinPts = 10)

library(factoextra)
fviz_cluster(Dbscan_cl1, DAT[, variables], geom = "point")
fviz_cluster(Dbscan_cl2, DAT[, variables], geom = "point")
fviz_cluster(Dbscan_cl3, DAT[, variables], geom = "point")
fviz_cluster(Dbscan_cl4, DAT[, variables], geom = "point")

###########
# k-MEANS #
###########

library(cluster)

kmeans.re <- kmeans(DAT[, variables], centers = 3, nstart = 20)
kmeans.re

kmeans.re1 <- kmeans(DAT[, variables], centers = 4, nstart = 20)
kmeans.re1

plot(DAT[, variables],
     col = kmeans.re$cluster, pch = 19,
     main = "K-means with 3 clusters")

plot(DAT[, variables],
     col = kmeans.re1$cluster, pch = 19,
     main = "K-means with 4 clusters")
