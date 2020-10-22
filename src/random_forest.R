
source("docs/talleres/utils.R")

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



# RANDOM FOREST -----------------------------------------------------------

# install.packages("randomForest")
library(randomForest)

rfmod <- randomForest(medv ~ ., data = train)
rfmod
summary(rfmod)

?randomForest
# Dos parámetros principales:
#  - mtry: número de predictores que se seleccionan aleatoriamente. 
#  - ntree: número de árboles (número de muestras bootstrap). 
# 
# Otros parámetros:
#   - importance: importancia de cada variable. (Por defecto a FALSE)
#   - nodesize: número de observaciones mínimas en los nodos terminales
#   - maxnodes: número máximo de nodos terminales en los árboles.

rfmod <- randomForest(medv ~ ., 
                      data = train,
                      importance = TRUE,
                      mtry = 4,
                      ntree = 1000)

rfmod

rfmod$mse
sqrt(mean(rfmod$mse))

# IMPORTANCIA DE VARIABLES
importance(rfmod)
varImpPlot(rfmod)
varImpPlot(rfmod, type = 1)

# Distintas configuraciones

ntree <- seq(from = 250, to = 2000, by = 250)
mtry <- seq(from = 1, to = ncol(train), by = 2)

grid <- expand.grid(ntree = ntree, mtry = mtry)

set.seed(123)
error <- c()
for (i in 1:nrow(grid)){
  cat("\nIteración", i, "de", nrow(grid))
  mod <- randomForest(medv ~ ., 
                      data = train,
                      mtry = grid$mtry[i],
                      ntree = grid$ntree[i]
                      )
  pred_rf <- predict(mod, newdata = test)
  
  error <- c(error, rmse(test$medv, pred_rf))
}

grid$error <- error

grid <- grid[order(error),]

library(ggplot2)

ggplot(grid,
       aes(x = as.factor(mtry), y = as.factor(ntree), fill = error)
       ) +
  geom_raster() + 
  scale_fill_continuous(trans = "reverse")
  


#' Ejercicio:
#' Cambia el gráfico anterior de forma que:
#'    - el eje x se llame "Número de predictores (mtry)"
#'    - el eje y se llame "Número de muestras bootstrap (ntree)"
#'    - el título sea "Configuración de parámetros de Random Forest"
#'    - aplica theme_minimal()
#'    - desaparezca la leyenda de la derecha
#'    - aparezca en cada casilla el error (redondeado a 2 cifras decimales)
