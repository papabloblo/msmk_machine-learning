#' 
#' EJEMPLO DE APLICACIÓN DE RIDGE REGRESSION Y LASSO
#' 
#' El ejemplo está inspirado en la sección 6.6 de
#' Introduction to Statistical Learning
#' 


#' Como todo en R, hay múltiples paquetes para hacer
#' una misma cosa. 
#' 
#' Vamos a utilizar el paquete glmnet
#' Si no lo tienes instalado, ejecuta primero:
#' 
#'     install.packages("glmnet")
#'     

library(glmnet)

#' Los datos que utilizaremos como ejemplo
#' están recogidos en el paquete ISLR
#' 

library(ISLR)

?Hitters

#' Queremos predecir el salario de un jugador de baseball
#' basándonos en características del jugador
#' 

#' Fíjate que la variable Salary (variable objetivo)
#' contiene datos ausentes
mean(is.na(Hitters$Salary))

#' Eliminamos las observaciones con esta variable ausente

Hitters <- na.omit(Hitters)
mean(is.na(Hitters$Salary))


#' La función glmnet solo admite una matriz como input.
#' Utilizamos la función model.matrix() para conseguirlo.
#' Esta función no solamente produce una matriz con los
#' predictores numéricos; también convierte las variables
#' cualitativas a variables dummy (1/0).

# Con [, -1] eliminamos el intercept
x <- model.matrix(Salary~., Hitters)[, -1]
y <- Hitters$Salary


# RIDGE REGRESSION --------------------------------------------------------

#' Recuerda que Ridge Regression añade un parámetro lambda
#' que controla la penalización.
#' Para buscar el mejor valor, debemos probar con
#' distintos valores.
#' Elegimos 100 valores desde 10**10 hasta 10**-2

lambda_grid <- 10**seq(10, -2, length = 100)

plot(1:length(lambda_grid), lambda_grid)

#' El parámetro alpha = 0 indica que 
#' queremos ajustar Ridge regression
ridge_mod <- glmnet(x, y, alpha = 0, lambda = lambda_grid)

plot(ridge_mod)

#' Por defecto, glmnet estandariza las variables,
#' ¿para qué era útil esto?

#' Cada valor de lambda tendrá asociado
#' unos coeficientes distintos.
#' En este caso tendremos una matriz de 20x100:
#' 20 variables por 100 valores de lambda

coef(ridge_mod)
dim(coef(ridge_mod))


#' Cuanto mayor sea lambda, 
#' más "penalizará" el modelo
#' y los coeficientes (su norma L2)
#' serán menores.

ridge_mod$lambda[50]
coef(ridge_mod)[, 50]

# Norma L2
sqrt(sum(coef(ridge_mod)[-1, 50]^2))

ridge_mod$lambda[60]
coef(ridge_mod)[, 60]

# Norma L2
sqrt(sum(coef(ridge_mod)[-1, 60]^2))

#' Podemos predecir utilizando
#' un valor de lambda concreto

predict(ridge_mod, s=50, type="coefficients")[1:20,]
predict(ridge_mod, s=50, newx = x)


#' EJERCICIO:
#' Divide el conjunto de datos en train y test (50%-50%).
#' Ajusta una regresión lineal con todas las variables.
#' Calcula el error cometido en test (RMSE)
#' 
#' Calcula el error cometido en test para cada valor de 
#' lambda en una Ridge Regression.


#' Como hemos visto, la validación cruzada nos aporta
#' un mecanismo para evaluar modelos.
#' En este paquete tenemos disponible la función cv.glmnet()

# Fijamos la semilla para que los resultados sean replicables
set.seed(1) 
train <- sample(1:nrow(x), nrow(x)/2)
test <- -train

#' Por defecto, cv.glmment() realiza 10 particiones
cv_out <- cv.glmnet(x[train, ], y[train], alpha = 0)

plot(cv_out)

best_lambda <- cv_out$lambda.min
best_lambda

ridge_pred <- predict(ridge_mod, s=best_lambda, newx = x[test,])
sqrt(mean((ridge_pred-y[test])**2))

#' Una vez elegido el parámetro óptimo de lambda,
#' entrenamos el modelo con todos los datos

ridge_mod <- glmnet(x, y, alpha = 0)

predict(ridge_mod, type="coefficients", s=best_lambda)[1:20,]

#' ¡Fíjate que ninguno de los coeficientes es 0!
#' Ridge regression no sirve para hacer selección de variables.


# LASSO -------------------------------------------------------------------

#' Para ajustar LASSO, podemos repetir lo anterior
#' utilizando alpha=1

lasso_mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = lambda_grid)

plot(lasso_mod)

set.seed(1)
cv_out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv_out)
best_lambda <- cv_out$lambda.min

lasso_pred <- predict(lasso_mod, x = best_lambda, newx = x[test,])

sqrt(mean((lasso_pred-y[test])**2))

out <- glmnet(x, y, alpha=1, lambda = lambda_grid)
lasso_coef <- predict(out, type = "coefficients", s = best_lambda)[1:20,]
lasso_coef
