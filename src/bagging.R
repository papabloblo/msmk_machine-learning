
# DATOS -------------------------------------------------------------------

# install.packages("mlbench")
data("BostonHousing", package = "mlbench")
?mlbench::BostonHousing


# TRAIN Y TEST ------------------------------------------------------------

set.seed(1234)
porc_train <- 0.75
id_train <- sample(1:nrow(BostonHousing), size = nrow(BostonHousing)*porc_train)
train <- BostonHousing[id_train, ]
test <- BostonHousing[-id_train, ]



# ÁRBOLES -----------------------------------------------------------------

library(rpart)
library(rpart.plot)
arbol <- rpart(medv ~ ., data = train)

rpart.plot::rpart.plot(arbol)

pred_arbol <- predict(arbol, newdata = test)

rmse_arbol <- sqrt(mean((test$medv - pred_arbol)**2))

rmse <- function(real, pred){
  return(sqrt(mean((real - pred)**2)))
}

rmse_arbol <- rmse(test$medv,pred_arbol)


# BAGGING -----------------------------------------------------------------

# install.packages("ipred")
library(ipred)

mod <- bagging(medv ~ ., data = train)
mod

# Número de muestras bootstrap
mod <- bagging(medv ~ ., data = train, nbagg = 50)
mod

# Cálculo de error OOB
mod <- bagging(medv ~ ., data = train, coob = TRUE)
mod

# ipred utiliza rpart, así que podemos controlar sus parámetros
mod <- bagging(medv ~ ., data = train, coob = TRUE, 
               control = rpart.control(maxdepth = 5))
mod


pred_bag <- predict(mod, newdata = test)

rmse_bag <- rmse(test$medv, pred_bag)

rmse_arbol
rmse_bag


# Distintas configuraciones del número de muestras 

set.seed(123)
b <- seq(20, 45, by = 5)
error <- c()
for (i in b){
  mod <- bagging(medv ~ ., data = train, nbagg = i, coob = TRUE)
  error <- c(error, mod$err)
}

error_bag <- data.frame(b = b, error = error)

library(ggplot2)

ggplot(error_bag,
       aes(x = b, y = error)
       ) +
  geom_line()
