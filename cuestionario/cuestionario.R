library(tidyverse)
library(tidymodels)
houses_train <- read_csv("cuestionario/houses_train.csv")


houses_train$sqft_living <- houses_train$sqft_living/10.764


houses_train %>% 
  ggplot(aes(x = grade, y  = price)) +
  geom_point(alpha = 0.1) +
  labs(
    title = "Precio vs superficie",
    x = "Superficie en metros cuadrados",
    y = "Precio"
  )

skimr::skim(houses_train)

library(leaflet)
leaflet(houses_train) %>% addTiles() %>% addCircleMarkers()



train_split <- initial_split(houses_train, prop = .80) 
train_split

# Conjunto de datos de entrenamiento
datos_train <- train_split %>% 
  training()

# Conjunto de datos de test
datos_test <- train_split %>% 
  testing()


lm1 <- lm(price~sqft_living, data = datos_train)
lm2 <- lm(price~sqft_living + grade, data = datos_train)
summary(lm1)
summary(lm2)



pred_test <- select(houses_train, price)



train_x <- select(datos_train, -price, -id, -date) %>% 
  as.matrix()

test_x <- select(datos_test, -price, -id, -date) %>% 
  as.matrix()



library(xgboost)

gradient_boost <- xgboost(data = train_x, 
                          label = datos_train$price,
                          nrounds = 1000,
                          params = list(eta = 0.01, 
                                        max_depth = 3,
                                        colsample_bytree = 0.75
                          )
)




pred_test$pred_bst <- predict(gradient_boost, test_x)

error <- error %>% 
  bind_rows(tibble(modelo = "bst", error = rmse(pred_test$price, pred_test$pred_bst)))
