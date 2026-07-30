#────────#
# Caso 1 #
#────────#

library(nlme)
library(performance)

dat <- read.table("TP5/tempcorr.txt", header = TRUE)
plot(abund ~ year, data = dat)
plot(abund ~ rain, data = dat)

# Modelo Lineal común (violación de supuestos)
m0 <- gls(abund ~ rain + year, na.action = na.omit, data = dat)
summary(m0)
AIC(m0)
plot(m0)
check_model(m0)
acf(m0$residuals)

# Modelo con autocorrelación de simetría compuesta
m1 <- gls(abund ~ rain + year, na.action = na.omit, data = dat,
          correlation = corCompSymm(form = ~ year))
summary(m1)
AIC(m1)

# Modelo con autocorrelación AR-1
m2 <- gls(abund ~ rain + year, na.action = na.omit, data = dat,
          correlation = corAR1(form = ~ year))
summary(m2)
AIC(m2)

AIC(m0, m1, m2)


#────────#
# Caso 2 #
#────────#

library(sp)
library(gstat)
library(nlme)

data(meuse)
meuse_df <- meuse # data frame
meuse_sp <- meuse # spatial object
coordinates(meuse_sp) <- ~x + y

# zinc tiene sesgo a la derecha
meuse_df$logzinc <- log(meuse_df$zinc)
meuse_sp$logzinc <- log(meuse_sp$zinc)

# Bubble plot
meuse_sp$scaled_logzinc <- log(meuse_sp$zinc) - mean(log(meuse_sp$zinc))
bubble(
  meuse_sp, "scaled_logzinc",
  main = "log(Zinc) concentration - Meuse floodplain",
  col = c("steelblue", "orangered")
)

# Comparar con distancia al río
bubble(
  meuse_sp, "dist",
  main = "Distance to river",
  col = c("darkgreen", "orange")
)

# Variograma de la variable respuesta
vgm_raw <- variogram(logzinc ~ 1, meuse_sp)
plot(vgm_raw, type = "b")

# Variograma de los residuos zinc ~ distancia al río
vgm_resid <- variogram(logzinc ~ dist, meuse_sp)
plot(vgm_resid, type = "b")

vgm_cloud <- variogram(logzinc ~ dist, meuse_sp, cloud = TRUE)
plot(vgm_cloud)

show.vgms() # modelos posibles

# modelos con diferente estructura espacial
m0 <- gls(logzinc ~ dist, data = meuse_df)

m_exp <- gls(
  logzinc ~ dist, data = meuse_df,
  correlation = corExp(form = ~x + y, nugget = TRUE)
)

m_gaus <- gls(
  logzinc ~ dist, data = meuse_df,
  correlation = corGaus(form = ~x + y, nugget = TRUE)
)

m_sph <- gls(
  logzinc ~ dist, data = meuse_df,
  correlation = corSpher(form = ~x + y, nugget = TRUE)
)

m_ratio <- gls(
  logzinc ~ dist, data = meuse_df,
  correlation = corRatio(form = ~x + y, nugget = TRUE)
)

m_lin <- gls(
  logzinc ~ dist, data = meuse_df,
  correlation = corLin(form = ~x + y, nugget = TRUE)
)

# Comparación de modelos
AIC(m0, m_exp, m_gaus, m_sph, m_ratio, m_lin)

# modelo ganador
summary(m_gaus)

# Comparación de los variogramas
var0 <- Variogram(
  m0, form = ~ x + y, robust = TRUE, resType = "normalized"
)
plot(var0)

var_gaus <- Variogram(
  m_gaus, form = ~ x + y, robust = TRUE, resType = "normalized"
)
plot(var_gaus)

var_exp <- Variogram(
  m_exp, form = ~ x + y, robust = TRUE, resType = "normalized"
)
plot(var_exp)
