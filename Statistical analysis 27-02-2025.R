
# Librerías
library(readxl)     
library(dplyr)      
library(ggplot2)    
library(gridExtra) 
library(AER)
library(MASS)

# Carga de base de datos
data <- read_excel("dataset 2702.xlsx")

# Modelos

## Egresos - Ancestría amerindia
cor(data$HDR, data$AMR)

m1 <- glm(
  HD ~ AMR,
  data = data,
  offset = log(data$POP2015),
  family = poisson)
dispersiontest(m1)

m1 <- glm.nb(
  HD ~ AMR + offset(log(POP2015)),
  data = data)
summary(m1)
exp(coef(m1))

m1.2 <- glm(
  HD ~ AMR + CDI + SMK100 + EPP,
  data = data,
  offset = log(POP2015),
  family = poisson)
dispersiontest(m1.2)

m1.2 <- glm.nb(
  HD ~ AMR + + CDI + SMK100 + EPP + offset(log(POP2015)),
  data = data)
summary(m1.2)
exp(coef(m1.2))

## Egresos - Ancestría mapuche

cor(data$HDR, data$MAP)

m2 <- glm(
  HD ~ MAP,
  data = data,
  offset = log(POP2015),
  family = poisson)
dispersiontest(m2)

m2 <- glm.nb(
  HD ~ MAP + offset(log(POP2015)),
  data = data)
summary(m2)
exp(coef(m2))

data$hd_pred2 <- predict(m2, type="response")
data$hdr_pred2 <- 100000*data$hd_pred2/data$POP2015

m2.2 <- glm(
  HD ~ MAP + CDI + SMK100 + EPP,
  data = data,
  offset = log(POP2015),
  family = poisson)
dispersiontest(m2.2)

m2.2 <- glm.nb(
  HD ~ MAP + CDI + SMK100 + EPP + offset(log(POP2015)),
  data = data)
summary(m2.2)
exp(coef(m2.2))


g2 <- ggplot(data, aes(x = MAP)) +
  geom_point(aes(y = HDR, color = "Observed"), alpha = 0.5) +
  geom_line(aes(y = hdr_pred2, color = "Predicted"), linewidth = 1) +
  labs(
    x = "Mapuche ancestry proportion",
    y = "Hospital discharge rate",
  ) +
  scale_color_manual(name = "", values = c("Observed" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "right",       # Posicionar la leyenda en la parte inferior
)

## Egresos - Ancestría aymara

cor(data$HDR, data$AYM)

m3 <- glm(
  HD ~ AYM,
  data = data,
  offset = log(POP2015),
  family = poisson)
dispersiontest(m3)

m3 <- glm.nb(
  HD ~ AYM + offset(log(POP2015)),
  data = data)
summary(m3)
exp(coef(m3))

data$hd_pred3 <- predict(m3, type="response")
data$hdr_pred3 <- 100000*data$hd_pred3/data$POP2015

m3.2 <- glm(
  HD ~ AYM + CDI + SMK100 + EPP,
  data = data,
  offset = log(POP2015),
  family = poisson)
dispersiontest(m3.2)

m3.2 <- glm.nb(
  HD ~ AYM + CDI + SMK100 + EPP + offset(log(POP2015)),
  data = data)
summary(m3.2)
exp(coef(m3.2))

g3 <- ggplot(data, aes(x = AYM)) +
  geom_point(aes(y = HDR, color = "Observed"), alpha = 0.5) +
  geom_line(aes(y = hdr_pred3, color = "Predicted"), size = 1) +
  labs(
    x = "Aymara ancestry proportion",
    y = "Hospital discharge rate",
  ) +
  scale_color_manual(name = "", values = c("Observed" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "right",       # Posicionar la leyenda en la parte inferior
 )

## Mortalidad - Ancestría amerindia

cor(data$SMR, data$AMR)

m4 <- glm(
  DC ~ AMR,
  data = data,
  offset = log(POP2016),
  family = poisson)
dispersiontest(m4)

m4 <- glm.nb(
  DC ~ AMR + offset(log(POP2016)),
  data = data)
summary(m4)
exp(coef(m4))

m4.2 <- glm(
  DC ~ AMR + CDI + SMK100 + EPP,
  data = data,
  offset = log(POP2016),
  family = poisson)
dispersiontest(m4.2)

m4.2 <- glm.nb(
  DC ~ AMR + CDI + SMK100 + EPP + offset(log(POP2016)),
  data = data)
summary(m4.2)
exp(coef(m4.2))


## Mortalidad - Ancestría mapuche

cor(data$SMR, data$MAP)

m5 <- glm(
  DC ~ MAP,
  data = data,
  offset = log(POP2016),
  family = poisson)
dispersiontest(m5)

m5 <- glm.nb(
  DC ~ MAP + offset(log(POP2016)),
  data = data)
summary(m5)
exp(coef(m5))

data$dc_pred2 <- predict(m5, type="response")
data$smr_pred2 <- 100000*data$dc_pred2/data$POP2016

m5.2 <- glm(
  DC ~ MAP + CDI + SMK100 + EPP,
  data = data,
  offset = log(POP2016),
  family = poisson)
dispersiontest(m5.2)

m5.2 <- glm.nb(
  DC ~ MAP + CDI + SMK100 + EPP + offset(log(POP2016)),
  data = data)
summary(m5.2)
exp(coef(m5.2))

g5 <- ggplot(data, aes(x = MAP)) +
  geom_point(aes(y = SMR, color = "Observed"), alpha = 0.5) +
  geom_line(aes(y = smr_pred2, color = "Predicted"), size = 1) +
  labs(
    x = "Mapuche ancestry proportion",
    y = "Mortality rate",
  ) +
  scale_color_manual(name = "", values = c("Observed" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "right",       # Posicionar la leyenda en la parte inferior
)


## Mortalidad - Ancestría aymara

cor(data$SMR, data$AYM)

m6 <- glm(
  DC ~ AYM,
  data = data,
  offset = log(POP2016),
  family = poisson)
dispersiontest(m6)

m6 <- glm.nb(
  DC ~ AYM + offset(log(POP2016)),
  data = data)
summary(m6)
exp(coef(m6))

data$dc_pred3 <- predict(m6, type="response")
data$smr_pred3 <- 100000*data$dc_pred3/data$POP2016

m6.2 <- glm(
  DC ~ AYM + CDI + SMK100 + EPP,
  data = data,
  offset = log(POP2016),
  family = poisson)
dispersiontest(m6.2)

m6.2 <- glm.nb(
  DC ~ AYM + CDI + SMK100 + EPP + offset(log(POP2016)),
  data = data)
summary(m6.2)
exp(coef(m6.2))

g6 <- ggplot(data, aes(x = AYM)) +
  geom_point(aes(y = SMR, color = "Observed"), alpha = 0.5) +
  geom_line(aes(y = smr_pred3, color = "Predicted"), size = 1) +
  labs(
    x = "Aymara ancestry proportion",
    y = "Mortality rate",
  ) +
  scale_color_manual(name = "", values = c("Observed" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "right",       # Posicionar la leyenda en la parte inferior
)

grid.arrange(g2, g3, g5, g6, nrow = 2)