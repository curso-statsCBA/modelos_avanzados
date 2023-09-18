## Práctico 3. Modelos Aditivos Generalizados

###############
### Caso 1. ###
###############

library(mgcv) 
library(gamm4)
library(gratia)
library(tidygam)
library(ggplot2)
library(patchwork)

RIKZ <- read.table("RIKZ.txt", header = TRUE)
#para calcular la riqueza sumamos las filas 2 a la 76
RIKZ$Richness <- rowSums(RIKZ[, 2:76] > 0)
#convertir "exposure" en factor
RIKZ$exposure <- as.factor(RIKZ$exposure)

#exploración
g1 <- ggplot(RIKZ, aes(x= NAP, y = Richness, color = exposure)) + 
  geom_point() + scale_fill_viridis_d()
g2 <- ggplot(RIKZ, aes(x= Richness, y = exposure, fill = exposure)) +
  geom_violin() + scale_fill_viridis_d()
g1 | g2

# GAM
# (no se muestra quasipoisson, tiene scale = 1.14)
# recordar siempre revisar la sobredispersión
fit1 <- gam(Richness ~ exposure + s(NAP), data = RIKZ, 
            family = poisson, method = "REML")
summary(fit1)

# prueba de supuestos del modelo
layout(matrix(1:4,2,2)) 
gam.check(fit1)
layout(1)

# visualización del spline
plot(fit1)
plot(fit1, seWithMean = TRUE, shade = TRUE, 
     shade.col = "palegreen", trans = exp)

# idem en gratia
sm1 <- smooth_estimates(fit1) |> add_confint() |> transform_fun(fun = exp)
ggplot(sm1, aes(NAP, est)) + geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
              alpha = 0.2, fill = "forestgreen")

# idem en tidygam
tp1 <- predict_gam(fit1, tran_fun = exp, length_out = 50)
plot(tp1, series = "NAP") 
ggplot(tp1, aes(NAP, Richness, color = exposure)) + geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, 
                  fill = exposure), alpha = 0.2)

# ¿Interacción con un factor?
# smooths separados para cada nivel
fit2 <- gam(Richness ~ exposure +  s(NAP, by = exposure),
            data = RIKZ, family = poisson, method = "REML")
summary(fit2)

layout(matrix(1:4,2,2))
gam.check(fit2)
layout(1)

layout(matrix(1:3, 1, 3)) # se esperan 3 splines
plot(fit2, seWithMean = TRUE, shade = TRUE, 
     shade.col = "palegreen", trans = exp)
layout(1)

sm2 <- smooth_estimates(fit2)
sm2 <- sm2 |> add_confint() |> transform_fun(fun = exp)

g2 <- ggplot(sm2, aes(x = NAP, y = est)) +  
  geom_line(colour = "forestgreen", linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
              alpha = 0.2, fill = "forestgreen") +
  facet_wrap( . ~ exposure)
g2 #gratia

# smooths con misma rugosidad
# (más adecuado para muchos niveles poco interesantes) 
fit3 <- gam(Richness ~ s(NAP, exposure, bs = "fs"),
            data = RIKZ, family = poisson, method = "REML")
summary(fit3)

layout(matrix(1:4,2,2))
gam.check(fit3)
layout(1)

sm3 <- smooth_estimates(fit3)
sm3 <- sm3 |> add_confint() |> transform_fun(fun = exp)
g3 <- ggplot(sm3, aes(x = NAP, y = est)) +  
  geom_line(colour = "forestgreen", linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
              alpha = 0.2, fill = "forestgreen") +
  facet_wrap( . ~ exposure)
g3 # gratia

# no hay método en tidygam para bs = fs
# el plot.gam de mgcv es bastante malo

# comparación de modelos
AIC(fit1, fit2, fit3)
anova(fit1, fit2, test = "Chisq")

# puedo poner un efecto random sencillo tipo (1 | Beach)
# entonces puedo usar todas las familias de gam!
fit_r <- gam(Richness ~ exposure + s(NAP) + s(Beach, bs = "re"), data = RIKZ, 
             family = poisson, method = "REML")

AIC(fit1, fit_r)
summary(fit_r)

# puedo hacer lo mismo en gamm4 (menos familias)
# y puedo poner efectos random más complejos
rm1 <- gamm4(Richness ~ exposure + s(NAP), random = ~ (1|Beach), data = RIKZ,
             family = poisson, REML = TRUE)
rm2 <- gamm4(Richness ~ exposure + s(NAP), 
             random = ~ (1|Beach) + (0+NAP|Beach), data = RIKZ,
             family = poisson, REML = TRUE)

AIC(rm1$mer, rm2$mer)
summary(rm1$gam)

sm4 <- smooth_estimates(rm1$gam)
sm4 <- sm4 |> add_confint() |> transform_fun(fun = exp)
g4 <- ggplot(sm4, aes(x = NAP, y = est)) +  
  geom_line(colour = "forestgreen", linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
              alpha = 0.2, fill = "forestgreen")
g4 # gratia 


##############
### Caso 2 ###
##############

cyc <- read.table("cyclop.txt", header = TRUE)
cyc<-na.omit(cyc) # mgcv no admite datos faltantes
colnames(cyc)<-c("nec", "flo", "lar", "fru", "PF", "pol", "PP")

# modelo isotrópico puramente aditivo
# la base por defecto de s() es "tp" thin plate spline
# selec = TRUE para DOBLE PENALIDAD (selección de variables)
m1 <- gam(pol ~ s(nec, k = 10, bs = "tp") + s(flo, k = 10, bs = "tp"), 
          data=cyc, family = poisson, method = "REML", select = TRUE)
summary(m1)

# examen del modelo
concurvity(m1)
layout(matrix(1:4,2,2))
gam.check(m1)
layout(1)

# gráfico para cada spline
layout(matrix(1:2,1,2)) # se esperan 2 splines
plot(m1, seWithMean = TRUE, shade = TRUE, 
     shade.col = "pink", trans = exp)
layout(1)

# gráfico para superficies via mgcv
vis.gam(m1, view = c("nec", "flo"), type = "response",
        plot.type = "contour", color = "cm")

# interacción. Isotrópico, sensible a escala 
m1 <- gam(pol ~ s(nec, k = 10, bs = "tp") + s(flo, k = 10, bs = "tp"), 
          data=cyc, family = poisson, method = "REML", 
          select = TRUE)
m2 <- gam(pol ~ s(nec, flo, k = 100, bs = "tp"), data=cyc, 
          family = poisson, method = "REML", 
          select = TRUE)
AIC(m1, m2)

summary(m2)
vis.gam(m2, view = c("nec", "flo"), type = "response", 
        plot.type = "contour", color = "cm") # ajá

# interacción. No isotrópico, invariantes a la escala
# Modelo aditivo, cambiamos la base a "cr" cubic spline
m3 <- gam(pol ~ s(nec, bs = "cr") + s(flo, bs = "cr"), 
          data=cyc, family = poisson, method = "REML", 
          select = TRUE)
m4 <- gam(pol ~ te(nec, flo), data = cyc, 
          family = poisson, method = "REML",
          select = TRUE)
m5 <- gam(pol ~ s(nec, bs = "cr") + s(flo, bs = "cr") + ti(nec, flo),
          data = cyc, family = poisson, method = "REML", 
          select = TRUE)
AIC(m3, m4, m5)

summary(m5)
vis.gam(m5, view = c("nec", "flo"), type = "response", 
        plot.type = "contour", color = "cm")

# gráfico para superficies via tidygam
m5_p <- predict_gam(m5, tran_fun = exp, length_out = 50)
ggplot(m5_p, aes(nec, flo, z = pol)) + 
  geom_raster(aes(fill = pol)) + 
  geom_contour(colour = "white") +
  scale_fill_viridis_c() +
  theme_minimal()

### END ###