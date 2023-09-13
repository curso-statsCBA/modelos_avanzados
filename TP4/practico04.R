## ejemplo 1

library(emmeans)
library(nlme)
library(multcomp)
library(ggplot2)

Csize_mF <- read.csv("floral_size.csv", header = TRUE, stringsAsFactors = TRUE)

## Modelo con nlme (normal, único efecto random posible)
Csize.field1 <- lme(log(CSF) ~ PopF, random = ~ 1 | indivF, data = Csize_mF, method = "REML") 
summary(Csize.field1)


## probar efectos fijos
Csize.field0 <- lme(log(CSF) ~ 1, random = ~ 1 | indivF, data = Csize_mF, method = "ML") 
Csize.field2 <- lme(log(CSF) ~ PopF, random = ~ 1 | indivF, data = Csize_mF, method = "ML") 
anova(Csize.field0, Csize.field2)

# modelo final
Csize.field3 <- lme(log(CSF) ~ PopF, random = ~ 1 | indivF, data = Csize_mF, method = "REML") 
summary(Csize.field3)

# diagnóstico
plot(Csize.field3)
qqnorm(resid(Csize.field3))

# pair-wise differences
CSgrafico_f <- emmeans(Csize.field3, list(pairwise ~ PopF), adjust = "tukey")
CSgrafico_f

# Floral shape in field - plot 
CS_campito <- plot(CSgrafico_f)
letrasF <- cld(CSgrafico_f, Letters = letters)
CS_F <- CS_campito + 
  geom_text(data = letrasF, aes(letrasF$emmean, letrasF$PopF, label = letrasF$.group),
            position = position_nudge(x = 0.05), 
            size = 4) +
  labs(x= "log(Centroid size)", y = "Natural populations") + 
  xlim(3.6,4.25) +
  theme_bw()

CS_F

## ejemplo 2

library(lme4)
library(ggplot2)

banded <- read.table("bandedSP.txt", header = TRUE)

# exploración
ggplot(banded, aes(x=Juncus,y=Banded))+geom_point()+facet_wrap(.~Site)

# diferentes modelos random (sólo ML)
m1 <- glmer(Banded ~ Juncus + (1|Site), data = banded, 
            family = poisson)
m2 <- glmer(Banded ~ Juncus + (0+Juncus|Site) + (1|Site), data = banded, 
            family = poisson)
# NOTA: la forma (Juncus|Site) no ajusta.

AIC(m1, m2)
BIC(m1, m2)

# diferentes modelos fijos
m3 <- glmer(Banded ~ 1 + (1|Site), data = banded, 
            family = poisson)
m4 <- glmer(Banded ~ Juncus + (1|Site), data = banded, 
            family = poisson)

anova(m3, m4)

summary(m4)

## Debemos chequear la sobredispersión siempre en el modelo más complejo

overdisp_fun <- function(m) {
  rdf <- df.residual(m)
  rp <- residuals(m,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(m2)

# diagnóstico
plot(m4)
qqnorm(resid(m4))

# si hubiera sobredispersión podríamos usar quasipoisson con
library(MASS)

q1 <- glmmPQL(Banded ~ Juncus, random = ~1|Site, data = banded, 
              family = poisson)

summary(q1)

