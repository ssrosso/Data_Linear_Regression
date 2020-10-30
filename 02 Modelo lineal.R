m <- lm(formula = monto ~ salario + cat_24m + n_hijos + edad, 
        data = catalogos)
m

summary(m)
coefficients(m)

str(m$coefficients)

X <- model.matrix(m)
Y <- catalogos$monto
str(X)
str(Y)

# X'X
t(X) %*% X

# Inversa (X'X)
solve(t(X) %*% X)

# Estimaciones bj
B <- solve(t(X) %*% X) %*% t(X) %*% Y

# Sumas de cuadados
T <- sum((Y - mean(Y))^2)


Q <- sum((m$residuals)^2)
sum((Y - m$fitted.values)^2)

S2 <- Q/m$df.residual
S2

S <- sqrt(S2)
S

R2 <- 1 - Q/T

# D(bj)
sqrt(diag(solve(t(X) %*% X)))*sqrt(S2)

##########################################################

# Matriz de correlaciones
head(X)
head(X[ , -1])
head(X[ , 2:5])
cor(X[, -1])
# Det
DET <- det(cor(X[, -1]))

# VIFs
# VIF son los elemntos de la diagonal de la matriz de correlaciones invertida
VIF <- diag(solve(cor(X[, -1])))

#######################################################

# H
H <- X %*% solve(t(X) %*% X)  %*%  t(X)

# hii
hii <- diag(H)
summary(hii)
hist(hii)
hii[hii > 3*5/347]

r_extudentizado <- m$residuals/(S*sqrt(1-hii))

plot(m$fitted.values, r_extudentizado)
abline(h=c(2, -2), col='red')
plot(m1)

m1 <- lm(
  formula = log(monto) ~ salario + cat_24m + n_hijos + edad, 
  data = catalogos)
summary(m1)


# Transformaciones de variables

catalogos$edad_2 <- catalogos$edad^2
catalogos$edad_cent_2 <- 
  (catalogos$edad - mean(catalogos$edad))^2

m3 <- lm(
  formula = log(monto) ~ salario + cat_24m + n_hijos + edad + edad_2, 
  data = catalogos)
summary(m3)

m4 <- lm(
  formula = 
    log(monto) ~ salario + cat_24m + n_hijos + edad + edad_cent_2, 
  data = catalogos)
summary(m4)

# Dummies

m5 <- lm(formula = log(monto) ~ salario + cat_24m +
           sexo + regionEste, 
         data = catalogos)
summary(m5)
head(model.matrix(m5))

catalogos$regionEste <- as.numeric(catalogos$regios == "Este")
