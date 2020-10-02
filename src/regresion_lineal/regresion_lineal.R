#' 
#' EJEMPLO DE REGRESIÓN LINEAL
#' 
#' El ejemplo está extraido de la sección 3.6 
#' del libro "Introduction to Statistical Learning"
#' (suele abreviarse como ISLR)
#' 


 
# DEPENDENCIAS ------------------------------------------------------------

# Paquete con una gran colección de datos y funciones
library(MASS)

# Paquete asociado al libro ISLR
library(ISLR)

# Paquete dedicado para gráficos
library(ggplot2)

rmse <- function(pred, real) {
  return(mean((pred-real)^2))
}

# PASO 1 ------------------------------------------------------------------

#' Utilizaremos el conjunto de datos `Boston`
#' Contiene información sobre 506 vecindarios en Boston
?Boston

names(Boston)

#' Queremos predecir la variable `medv` 
#' (precio mediano del valor de una casa en miles de dólares)
#' usando 13 predictores.
#' 
#' Más adelante hablaremos en detalle de métodos para evaluar el comportamiento
#' de un modelo. De momento solo separaremos el conjunto de datos en dos mitades

id_train <- sample(1:nrow(Boston), size = nrow(Boston)/2)

boston_train <- Boston[id_train,]
boston_test <- Boston[-id_train,]


# REGRESIÓN LINEAL SIMPLE -------------------------------------------------

#' Comenzamos utilizando solamente la variable `lstat` 
#' (porcentaje de hogares con un bajo estatus socioeconómico).
#' 
#' Como solo utilizamos dos variables, podemos representar la
#' relación mediante un gráfico de dispersión

ggplot(data = boston_train, aes(x = lstat, y = medv)) +
  geom_point()

#' Ajustamos la regresión mediante la función lm() 

lm_fit <- lm(medv ~ lstat,data = boston_train)

lm_fit

summary(lm_fit)

names(lm_fit)

#' La recta de regresión se puede representar 
#' en ggplot2 conociendo el intercept y la pendiente (slope)
ggplot(data = boston_train, aes(x = lstat, y = medv)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = lm_fit$coefficients[1],
              slope = lm_fit$coefficients[2],
              color = "firebrick", 
              size = 2
              ) +
  theme_minimal()

#' Ya sabemos que un gráfico fundamental 
#' es el que enfrente los valores predichos
#' con los residuos.
#' 

resid <- data.frame(residuals = lm_fit$residuals,
                    pred = lm_fit$fitted.values)

ggplot(data = resid, aes(x = pred, y = residuals)) +
  geom_point() + 
  geom_hline(yintercept = 0, 
             color = "firebrick",
             size = 2
             )

plot(lm_fit)

par(mfrow = c(2, 2))
plot(lm_fit)
par(mfrow = c(1, 1))

#' A la vista del gráfico de residuos, hay cierta
#' evidencia de no linealidad.


#' Podemos predecir el conjunto de datos boston_test
#' 

pred1 <- predict(lm_fit, newdata = boston_test)

rmse1 <- rmse(pred1, boston_test$medv)

# REGRESIÓN LINEAL MÚLTIPLE -----------------------------------------------

lm_fit2 <- lm(medv ~ lstat + age, data = boston_train)
summary(lm_fit2)

#' El número de variables puede ser alto por lo que
#' tenemos un atajo para no tener que escribir
#' y ~ x1 + x2 + x3 + ...
 
lm_fit3 <- lm(medv ~ ., data = boston_train)
summary(lm_fit3)

#' También podemos excluir alguna variable

lm_fit4 <- lm(medv ~ .-age, data = boston_train)
summary(lm_fit4)

pred2 <- predict(lm_fit2, newdata = boston_test)
pred3 <- predict(lm_fit3, newdata = boston_test)
pred4 <- predict(lm_fit4, newdata = boston_test)

rmse2 <- rmse(pred2, boston_test$medv)
rmse3 <- rmse(pred3, boston_test$medv)
rmse4 <- rmse(pred4, boston_test$medv)

# TÉRMINOS DE INTERACCIÓN -------------------------------------------------

#' La sintaxis lstat:black le dice a R que incluya un
#' término de interacción entre las variables lstat y black.
#' 
#' La sintaxis lstat*age es un atajo para escribir 
#' lstat + age + lstat:age
#' 
lm_fit5 <- lm(medv ~ lstat*age, data = boston_train)
summary(lm_fit5)

pred5 <- predict(lm_fit5, newdata = boston_test)
rmse5 <- rmse(pred5, boston_test$medv)



# TRANSFORMACIONES NO LINEALES --------------------------------------------

#' Podemos incluir transformaciones no lineales 
#' mediante la función I()

lm_fit6 <- lm(medv ~ lstat + I(lstat^2), data = boston_train)
summary(lm_fit6)

par(mfrow = c(2, 2))
plot(lm_fit6)
par(mfrow = c(1, 1))

pred6 <- predict(lm_fit6, newdata = boston_test)
rmse6 <- rmse(pred6, boston_test$medv)


lm_fit7 <- lm(medv ~ . + I(lstat^2), data = boston_train)

pred7 <- predict(lm_fit7, newdata = boston_test)
rmse7 <- rmse(pred7, boston_test$medv)

# COMPARATIVA DE MODELOS --------------------------------------------------

rmse_fit <- data.frame(
  mod = c("lm_fit1", "lm_fit2", "lm_fit3", "lm_fit4","lm_fit5", "lm_fit6", "lm_fit7"),
  rmse = c(rmse1, rmse2, rmse3, rmse4, rmse5, rmse6, rmse7)
)

rmse_fit <- rmse_fit[order(rmse_fit$rmse), ]

plot(Boston)
