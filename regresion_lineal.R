library(readxl)

ventas <- read_xlsx("regresion_lineal.xlsx")

mod <- lm(vta ~ traf + preA + preB + preC + pub + promo + exhib, 
        data = ventas)

X <- model.matrix(mod)
Y <- ventas$vta

# Estimaciones bj
B <- solve(t(X) %*% X) %*% t(X) %*% Y

# Variacion Total
T <- sum((Y - mean(Y))^2)

#Variacion Residual
Q <- sum((mod$residuals)^2)

# Verificación
sum((Y - mod$fitted.values)^2)

# Coeficiente de determinacion
R2 <- 1 - Q/T
# R2 = 0.869 (alto % de T explicado por el modelo)

#Varianza de la perturbación
S2 <- Q/mod$df.residual
# S2 = 1621.55

# Analisis
summary(mod)
# R2 = 0.869
# R2 ajustado = 0,861 (bueno)
# Test de hipotesos sobre parametros: suponiendo alfa* = 0.05
# preC y exhib muestran que no son significativos


###################################################################


# Multicolinealidad
DET <- det(cor(X[, -1]))
# Como DET < 0.1, procedo a ver los VIFs

# VIFs
# Elemntos de la diagonal de la matriz de correlaciones invertida
VIF <- diag(solve(cor(X[, -1])))
# preA y preB presentan VIF > 10, entonces saco una (elijo preB)
# Chequeo nuevo DET
DET <- det(cor(X[, c(-1,-3)]))
# DET > 0.2, entonces la multicolinealidad no afecta las estimaciones


# Calculo nuevamente todo

mod2 <- lm(vta ~ traf + preA + preC + pub + promo + exhib, 
          data = ventas)

summary(mod2)
# R2 = 0.849
# R2 ajustado = 0,841 (bueno)
# Test de hipotesos sobre parametros: suponiendo alfa* = 0.05
# preC y exhib muestran que no son significativos


###################################################################


# Transformacion de variables

#Pruebo preC^2
ventas$preC2 <- (ventas$preC-mean(ventas$preC))^2

mod3 <- lm(vta ~ traf + preA + preC + pub + promo + exhib + preC2, 
           data = ventas)

summary(mod3)
#Sigue dando mal

# Pruebo preC^3
ventas$preC3 <- (ventas$preC-mean(ventas$preC))^3

mod4 <- lm(vta ~ traf + preA + preC + pub + promo + exhib + preC3, 
           data = ventas)

summary(mod4)
# Mejora pero sigue mal, asi que la saco

# Como no puedo transformar exhib, pruebo eliminandola
mod5 <- lm(vta ~ traf + preA + pub + promo, 
           data = ventas)

summary(mod5)
# R2 = 0.848
# R2 ajustado = 0.843
# Parametros significativos

###################################################################

X5 <- model.matrix(mod5)

# H
H <- X5 %*% solve(t(X5) %*% X5)  %*%  t(X5)

# hii
hii <- diag(H)
summary(hii)
hist(hii)
hii[hii > 3*5/120]

# Ningun punto presenta un hii por encima de lo esperado

Q5 <- sum((mod5$residuals)^2)
S5 <- sqrt(Q/mod5$df.residual)

r_extudentizado <- mod5$residuals/(S5*sqrt(1-hii))

plot(mod5$fitted.values, r_extudentizado)
abline(h=c(2, -2), col='red')

# Hay 3 puntos (1 punto mas que los demas) que son outliers pero no influyen

