
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

# Para aplicar boosting necesitamos hacer lo siguiente:
train_x <- train
train_x$medv <- NULL
train_x$chas <- as.numeric(train_x$chas)
train_x <- as.matrix(train_x)

train_y <- train$medv

test_x <- test
test_x$medv <- NULL
test_x$chas <- as.numeric(test_x$chas)
test_x <- as.matrix(test_x)

test_y <- test$medv

# BAGGING -----------------------------------------------------------------

# install.packages("xgboost")
library(xgboost)

boost_mod <- xgboost(data = train_x, 
                     label = train_y,
                     nrounds = 50
                     )

pred_boost <- predict(boost_mod, test_x)

rmse(test_y, pred_boost)

?xgboost
# Parámetros principales:
#  - eta: es el parámetro de shrinkage. 
#      En las diapositivas lo hemos llamado lambda
#  - max_depth: profundidad máxima del árbol. 
#  - subsample: porcentaje de observaciones que se utilzan en cada iteración.
#  - colsample_bytree: porcentaje de columnas en cada iteración (como en random forest).
#  - objective: especifica qué tipo de modelo hay que usar:
#       - reg:squearederror: regresión
#       - binary:logistic: clasificación binaria
#       - (...hay más...)
# 


boost_mod <- xgboost(data = train_x, 
                     label = train_y,
                     nrounds = 50,
                     params = list(
                       eta = 0.1,
                       max_depth = 3,
                       subsample = 0.5,
                       colsample_bytree = 0.4
                       )
                     )

# Es útil conocer qué resultado se obtendría en test en cada iteración.
dtrain <- xgb.DMatrix(train_x, label = train_y)
dtest <- xgb.DMatrix(test_x, label = test_y)

boost_mod <- xgb.train(data = dtrain, 
                     nrounds = 50,
                     params = list(
                       eta = 0.1,
                       max_depth = 3,
                       subsample = 0.5,
                       colsample_bytree = 0.4
                     ),
                     watchlist = list(train = dtrain, eval = dtest)
)


boost_mod <- xgb.train(data = dtrain, 
                       nrounds = 1000,
                       params = list(
                         eta = 0.1,
                         max_depth = 3,
                         subsample = 0.5,
                         colsample_bytree = 0.5
                       ),
                       watchlist = list(train = dtrain, eval = dtest),
                       early_stopping_rounds = 50
                       )


pred_boost <- predict(boost_mod, dtest)
rmse(test_y, pred_boost)

import <- xgb.importance(model = boost_mod)

xgb.plot.importance(import)
