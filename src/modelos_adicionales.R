library(gam)

houses_training2 <- houses_training %>% 
  mutate(anyo = YrSold - YearRemodAdd)

houses_test2 <- houses_test %>% 
  mutate(anyo = YrSold - YearRemodAdd)
mod_gam <- gam(SalePrice ~ s(GrLivArea, 4) + s(OverallQual, 4) + s(TotalBsmtSF, 4) + s(anyo, 4), data = houses_training2)


test_prediction$gam <- predict(mod_gam, newdata = houses_test2)


modelo_error <- bind_rows(modelo_error,
                          list(modelo = "gam", 
                               error = error(test_prediction$SalePrice, test_prediction$gam))
)
modelo_error

library(FNN)

miau <- knn.reg(
  train = train_x, 
  test = test_x, 
  y = houses_training$SalePrice,
  k = 50)

miau





test_prediction$knn <- miau$pred


modelo_error <- bind_rows(modelo_error,
                          list(modelo = "knn", 
                               error = error(test_prediction$SalePrice, test_prediction$knn))
)
modelo_error




modelo_error <- bind_rows(modelo_error,
                          list(modelo = "reglineal_glmnet_log", 
                               error = error(log(test_prediction$SalePrice), 
                                             log(test_prediction$predict_reglineal_glmnet_log)
                                             )
                               )
                          )
modelo_error

