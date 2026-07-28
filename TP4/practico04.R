#─────────────#
## ejemplo 1 ##
#─────────────#

library(emmeans)
library(nlme)
library(multcomp)
library(ggplot2)
library(performance)

CS <- read.csv("TP4/floral_size.csv", header = TRUE, stringsAsFactors = TRUE)

## Modelo con nlme (normal, único efecto random posible)
f1 <- lme(log(CSF) ~ PopF, random = ~ 1 | indivF, data = CS, method = "REML")
summary(f1)

## probar efectos fijos
f0 <- lme(log(CSF) ~ 1, random = ~ 1 | indivF, data = CS, method = "ML")
f2 <- lme(log(CSF) ~ PopF, random = ~ 1 | indivF, data = CS, method = "ML")
anova(f0, f2)

# modelo final
f3 <- lme(log(CSF) ~ PopF, random = ~ 1 | indivF, data = CS, method = "REML")
summary(f3)

# diagnóstico
check_model(f3)

# pair-wise differences
CS_f <- emmeans(f3, list(pairwise ~ PopF), adjust = "tukey")
CS_f

# Floral shape in field - plot
CS_campito <- plot(CS_f)
letrasF <- cld(CS_f, Letters = letters)
CS_F <- CS_campito +
  geom_text(
    data = letrasF,
    aes(letrasF$emmean, letrasF$PopF, label = letrasF$.group),
    position = position_nudge(x = 0.05),
    size = 4
  ) +
  labs(x = "log(Centroid size)", y = "Natural populations") +
  xlim(3.6, 4.25) +
  theme_bw()
CS_F

#─────────────#
## ejemplo 2 ##
#─────────────#

library(lme4)
library(ggplot2)

banded <- read.table("TP4/bandedSP.txt", header = TRUE)

# exploración
ggplot(banded, aes(x = Juncus, y = Banded)) +
  geom_point() +
  facet_wrap(. ~ Site)

# diferentes modelos random (sólo ML)
m1 <- glmer(
  Banded ~ Juncus + (1 | Site),
  data = banded, family = poisson
)
m2 <- glmer(
  Banded ~ Juncus + (0 + Juncus | Site) + (1 | Site),
  data = banded, family = poisson
)
m3 <- glmer(
  Banded ~ Juncus + (Juncus | Site),
  data = banded, family = poisson
)
# NOTA: la forma (Juncus|Site) arroja error por overfitting.

AIC(m1, m2)
BIC(m1, m2)

# diferentes modelos fijos
m4 <- glmer(
  Banded ~ 1 + (1 | Site), data = banded,
  family = poisson
)
m5 <- glmer(
  Banded ~ Juncus + (1 | Site), data = banded,
  family = poisson
)

anova(m4, m5)

summary(m5)

## Debemos chequear la sobredispersión siempre en el modelo más complejo
## ver limitaciones de esta función

overdisp_fun <- function(m) {
  rdf <- df.residual(m)
  rp <- residuals(m, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

overdisp_fun(m5)
overdisp_fun(m2) # comparar con el más complejo posible

# diagnóstico
plot(m5)
qqnorm(resid(m5))

# si hubiera sobredispersión podríamos usar quasipoisson con
# modelos quasi
library(MASS)

m5_qp <- glmmPQL(
  Banded ~ Juncus, random = ~1 | Site,
  data = banded, family = poisson)

summary(m5_qp)

# otras familias (binomial negativa en este caso)
library(glmmTMB)
library(DHARMa)

m5_nb <- glmmTMB(
  Banded ~ Juncus + (1 | Site),
  data = banded, family = nbinom2
)

summary(m5_nb)

check_model(m5_nb)
check_model(m5)

simres_nb <- simulateResiduals(m5_nb)
plot(simres_nb)

simres <- simulateResiduals(m5)
plot(simres)
