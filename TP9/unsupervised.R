library(mclust)
library(vegan)
PCscores <- read.table("pc_scores.txt", header = T)
xyscores <- read.table("coordenadas_planta.txt", header = T)
DAT <- data.frame(PCscores, xyscores[, 4:31])

# gaussian mixture analysis with 2 PC
variables <- c("PC1", "PC2")
BIC <- mclustBIC(DAT[, variables])
plot(BIC)
summary(BIC)

# best model
mod1 <- Mclust(DAT[, variables], x = BIC)
summary(mod1, parameters = T)

# second model
mod2 <- Mclust(DAT[, variables], G = 3, modelNames = "EVV")
summary(mod2, parameters = T)

# third model
mod3 <- Mclust(DAT[, variables], G = 4, modelNames = "EVE")
summary(mod3, parameters = T)

plot(mod1, what = "classification")

plot(mod2, what = "classification")

plot(mod3, what = "classification")

#comparación entre la clasificación a priori y con GMA
table(DAT$class, mod1$classification)

## gaussian mixture analysis with 2 PC & log centroid size
variables2 <- c("PC1", "PC2", "Log_Centroid_Size")
BIC2 <- mclustBIC(DAT[, variables2])
plot(BIC2)
summary(BIC2)# 7 unidades de BIC con el segundo

mod4 <- Mclust(DAT[, variables2], x = BIC)
summary(mod4, parameters = T)

plot(mod4, what = "classification")
plot(mod4, what = "uncertainty")
plot(mod4, what = "density")

#comparación entre la clasificación a priori y con GMA
table(DAT$class, mod4$classification)

#############################################
colores <- as.character(DAT$class)
colores[colores == "Pen"] <- "black"
colores[colores == "Lax"] <- "black"
colores[colores == "Hib"] <- "black"

simbol <-as.character(DAT$class)
simbol[simbol == "Pen"] <- "17"
simbol[simbol == "Lax"] <- "19"
simbol[simbol == "Hib"] <- "0"
simbol <- as.numeric(simbol)

plot(DAT[, c("PC1", "PC2")], type = "n")
ordihull(DAT[, c("PC1", "PC2")], groups = mod1$classification, draw = "polygon", col = "grey", lwd=0.1)
points(DAT[, c("PC1", "PC2")], col = colores, pch = simbol)


## DBSCAN
library(fpc)

Dbscan_cl <- dbscan(DAT[, variables], eps = 0.45, MinPts = 5)

Dbscan_cl1 <- dbscan(DAT[, variables], eps = 0.45, MinPts = 2)

# Plotting Cluster
plot(Dbscan_cl, DAT[, variables], main = "DBScan")
plot(Dbscan_cl1, DAT[, variables], main = "DBScan")

library(factoextra)
fviz_cluster(Dbscan_cl1, DAT[, variables], geom = "point")

### k-MEANS
library(ClusterR)
library(cluster)

kmeans.re <- kmeans(DAT[, variables], centers = 3, nstart = 20)
kmeans.re

kmeans.re1 <- kmeans(DAT[, variables], centers = 4, nstart = 20)
kmeans.re1

plot(DAT[, variables], 
     col = kmeans.re$cluster, 
     main = "K-means with 3 clusters")

plot(DAT[, variables], 
     col = kmeans.re1$cluster, 
     main = "K-means with 4 clusters")


