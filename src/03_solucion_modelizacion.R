library(tidyverse)


# PASO 1 ------------------------------------------------------------------

# Apartado 1
train <- readRDS(file = "taller_calidad_aire/data/train.RDS")
test <- readRDS(file = "taller_calidad_aire/data/test.RDS")

# Apartado 2
train <- train[2:nrow(train),]

# Apartado 3
train$fecha <- NULL
test$fecha <- NULL



# PASO 2 ------------------------------------------------------------------

mod_baseline <- lm(pm25~pm25_lag, data = train)

rmse <- function(real, pred){
  return(sqrt(mean((real - pred)**2)))
}

pred_baseline <- predict(mod_baseline, test)
rmse_baseline <- rmse(test$pm25, pred_baseline)


# PASO 3 ------------------------------------------------------------------

# Apartado 1
library(ipred)

set.seed(123)
b <- seq(50, 200, by = 50)
error <- c()
for (i in b){
  mod <- bagging(pm25 ~ ., data = train, nbagg = i)
  error <- c(error, rmse(test$pm25, predict(mod, test)))
}

mod_bagging <- bagging(pm25 ~ ., data = train, nbagg = b[which.min(error)])
pred_bagging <- predict(mod_bagging, test)
rmse_bagging <- rmse(test$pm25, predict(mod, test))

# Apartado 2
library(randomForest)
ntree <- seq(from = 100, to = 500, by = 100)
mtry <- seq(from = 2, to = ncol(train), by = 3)


grid <- expand.grid(ntree = ntree, mtry = mtry)

set.seed(123)
error <- c()
for (i in 1:nrow(grid)){
  cat("\nIteración", i, "de", nrow(grid))
  mod <- randomForest(pm25 ~ ., 
                      data = train,
                      mtry = grid$mtry[i],
                      ntree = grid$ntree[i]
  )
  pred_rf <- predict(mod, newdata = test)
  
  error <- c(error, rmse(test$pm25, pred_rf))
}


min_grid <- which.min(error)
mod <- randomForest(pm25 ~ ., 
                    data = train,
                    mtry = grid$mtry[min_grid],
                    ntree = grid$ntree[min_grid]
)

pred_rf <- predict(mod, newdata = test)

rmse_rf <- rmse(test$pm25, pred_rf)

# Apartado 3
library(xgboost)

train_x <- train
train_x$pm25 <- NULL
train_x <- as.matrix(train_x)

train_y <- train$pm25

test_x <- test
test_x$pm25 <- NULL
test_x <- as.matrix(test_x)

test_y <- test$pm25


dtrain <- xgb.DMatrix(train_x, label = train_y)
dtest <- xgb.DMatrix(test_x, label = test_y)

eta <- c(0.001, 0.01, 0.1)
max_depth <- c(1, 5, 7)
subsample <- c(0.3, 0.7)
colsample_bytree <- c(0.1, 0.5, 1)

grid <- expand.grid(eta = eta,
                    max_depth = max_depth,
                    subsample = subsample,
                    colsample_bytree = colsample_bytree)

set.seed(123)
error <- c()
for (i in 1:nrow(grid)){
  cat("\nIteración", i, "de", nrow(grid))
  
  
  boost_mod <- xgb.train(data = dtrain, 
                         nrounds = 1000,
                         params = list(
                           eta = grid$eta[i],
                           max_depth = grid$max_depth[i],
                           subsample = grid$subsample[i],
                           colsample_bytree = grid$colsample_bytree[i]
                         ),
                         watchlist = list(train = dtrain, eval = dtest),
                         early_stopping_rounds = 50
  )
  
  pred_boost <- predict(boost_mod, newdata = dtest)
  
  error <- c(error, rmse(test$pm25, pred_boost))
}


min_grid <- which.min(error)
boost_mod <- xgb.train(data = dtrain, 
                       nrounds = 1000,
                       params = list(
                         eta = grid$eta[min_grid],
                         max_depth = grid$max_depth[min_grid],
                         subsample = grid$subsample[min_grid],
                         colsample_bytree = grid$colsample_bytree[min_grid]
                       ),
                       watchlist = list(train = dtrain, eval = dtest),
                       early_stopping_rounds = 50
)

pred_boost <- predict(boost_mod, newdata = dtest)

rmse_boost <- rmse(test$pm25, pred_boost)



# PASO 4 ------------------------------------------------------------------

test %>% 
  mutate(
    fecha = lubridate::make_date(year = ano, month = mes, day = dia)
  ) %>% 
  ggplot(aes(x = fecha)) + 
  geom_line(aes(y = pm25), alpha = 0.5) +
  geom_line(aes(y = pred_boost), color = "firebrick") +
  labs(
    title = "Comparativa del modelo baseline con el valor real",
    subtitle = "La línea roja muestra la predicción de baseline",
    caption = "Minería de datos II - UFV",
    x = ""
    
  ) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b") +
  theme_minimal()


# PASO 5 ------------------------------------------------------------------

# Apartado 1
train <- readRDS(file = "taller_calidad_aire/data/train.RDS")
test <- readRDS(file = "taller_calidad_aire/data/test.RDS")

# Apartado 2
train <- train[2:nrow(train),]
dias_laborables <- readRDS("taller_calidad_aire/data/dias_laborables.RDS")

train <- train %>% left_join(dias_laborables)
test <- test %>% left_join(dias_laborables)

train$fecha <- NULL
test$fecha <- NULL


mod_baseline2 <- lm(pm25 ~ pm25_lag + laborable, data = train)


pred_baseline2 <- predict(mod_baseline2, test)
rmse_baseline2 <- rmse(test$pm25, pred_baseline2)



# Apartado 1
library(ipred)

set.seed(123)
b <- seq(50, 200, by = 50)
error <- c()
for (i in b){
  mod <- bagging(pm25 ~ ., data = train, nbagg = i)
  error <- c(error, rmse(test$pm25, predict(mod, test)))
}

mod_bagging2 <- bagging(pm25 ~ ., data = train, nbagg = b[which.min(error)])
pred_bagging2 <- predict(mod_bagging2, test)
rmse_bagging2 <- rmse(test$pm25, predict(mod_bagging2, test))

# Apartado 2
library(randomForest)
ntree <- seq(from = 100, to = 500, by = 100)
mtry <- seq(from = 2, to = ncol(train), by = 3)

grid <- expand.grid(ntree = ntree, mtry = mtry)

set.seed(123)
error <- c()
for (i in 1:nrow(grid)){
  cat("\nIteración", i, "de", nrow(grid))
  mod <- randomForest(pm25 ~ ., 
                      data = train,
                      mtry = grid$mtry[i],
                      ntree = grid$ntree[i]
  )
  pred_rf <- predict(mod, newdata = test)
  
  error <- c(error, rmse(test$pm25, pred_rf))
}


min_grid <- which.min(error)
mod_rf2 <- randomForest(pm25 ~ ., 
                    data = train,
                    mtry = grid$mtry[min_grid],
                    ntree = grid$ntree[min_grid]
)

pred_rf2 <- predict(mod_rf2, newdata = test)

rmse_rf2 <- rmse(test$pm25, pred_rf2)

# Apartado 3
library(xgboost)

train_x <- train
train_x$pm25 <- NULL
train_x <- as.matrix(train_x)

train_y <- train$pm25

test_x <- test
test_x$pm25 <- NULL
test_x <- as.matrix(test_x)

test_y <- test$pm25


dtrain <- xgb.DMatrix(train_x, label = train_y)
dtest <- xgb.DMatrix(test_x, label = test_y)

eta <- c(0.001, 0.01, 0.1)
max_depth <- c(1, 5, 7)
subsample <- c(0.3, 0.7)
colsample_bytree <- c(0.1, 0.5, 1)

grid <- expand.grid(eta = eta,
                    max_depth = max_depth,
                    subsample = subsample,
                    colsample_bytree = colsample_bytree)

set.seed(123)
error <- c()
for (i in 1:nrow(grid)){
  cat("\nIteración", i, "de", nrow(grid))
  
  
  boost_mod <- xgb.train(data = dtrain, 
                         nrounds = 1000,
                         params = list(
                           eta = grid$eta[i],
                           max_depth = grid$max_depth[i],
                           subsample = grid$subsample[i],
                           colsample_bytree = grid$colsample_bytree[i]
                         ),
                         watchlist = list(train = dtrain, eval = dtest),
                         early_stopping_rounds = 50
  )
  
  pred_boost <- predict(boost_mod, newdata = dtest)
  
  error <- c(error, rmse(test$pm25, pred_boost))
}


min_grid <- which.min(error)
boost_mod2 <- xgb.train(data = dtrain, 
                       nrounds = 1000,
                       params = list(
                         eta = grid$eta[min_grid],
                         max_depth = grid$max_depth[min_grid],
                         subsample = grid$subsample[min_grid],
                         colsample_bytree = grid$colsample_bytree[min_grid]
                       ),
                       watchlist = list(train = dtrain, eval = dtest),
                       early_stopping_rounds = 50
)

pred_boost2 <- predict(boost_mod2, newdata = dtest)

rmse_boost2 <- rmse(test$pm25, pred_boost2)


import <- xgb.importance(model = boost_mod2)

xgb.plot.importance(import)


rmses <- tribble(
  ~modelo, ~rmse,
  
  "baseline", rmse_baseline,
  "baseline2", rmse_baseline2,
  
  "bagging", rmse_bagging,
  "bagging2", rmse_bagging2,
  
  "random forest", rmse_rf,
  "random forest2", rmse_rf2,
  
  "boosting", rmse_boost,
  "boosting2", rmse_boost2,
  
)

saveRDS(rmses, "taller_calidad_aire/data/rmse.RDS")




