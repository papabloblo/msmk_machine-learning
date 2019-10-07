# EXPLICATIVIDAD ----------------------------------------------------------

# Algo en lo que se está trabajando activamente en el mundo
# del machine learning es en tratar de hacer que los 
# modelos no sean cajas negras.

library(patchwork)

p1 <- test_prediction %>% 
  ggplot(
    aes(x = SalePrice,
        y = predict_reglineal_glmnet_log - SalePrice)
    ) +
  geom_point()


test_prediction %>% 
  ggplot(
    aes(x = SalePrice,
        y = predict_reglineal_glmnet_log)
  ) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)



# Gradient boosting
p2 <- test_prediction %>% 
  ggplot(
    aes(x = SalePrice,
        y = gradient_boosting - SalePrice)
  ) +
  geom_point()



test_prediction %>% 
  ggplot(
    aes(x = SalePrice)
  ) +
  geom_point(aes(y = gradient_boosting - SalePrice), color = "steelblue", alpha = 0.5) +
  geom_point(aes(y = predict_reglineal_glmnet_log - SalePrice), color = "firebrick", alpha = 0.5)


test_prediction %>% 
  ggplot(
    aes(x = SalePrice,
        y = gradient_boosting)
  ) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)


p1 + p2




test_prediction %>% 
  transmute(
    SalePrice,
    res_bst = gradient_boosting - SalePrice,
    res_reg = predict_reglineal_glmnet_log - SalePrice,
  ) %>% 
  gather(key = "modelo", value = "residuo", -SalePrice) %>% 
  ggplot() +
  geom_density(aes(x = residuo, fill = modelo), alpha = 0.5)


test_prediction %>% 
  transmute(
    SalePrice,
    res_bst = gradient_boosting - SalePrice,
    res_reg = predict_reglineal_glmnet_log - SalePrice,
  ) %>% 
  gather(key = "modelo", value = "residuo", -SalePrice) %>% 
  ggplot() +
  geom_point(aes(x = SalePrice, y = residuo, color = modelo), alpha = 0.75) +
  geom_hline(yintercept = 0)


# install.packages("iml")

library(iml)

boost.GrLivArea <- Partial$new(predictor.boost, "")








library(DALEX)
library(ceterisParibus)
explainer_bst <- explain(gradient_boost, 
                         data = test_x,
                         y = houses_test$SalePrice)

explainer_reg <- explain(reglineal_glmnet_log, 
                         data = train_x,
                         y = houses_training$SalePrice,
                         predict_function = function(model, new_data) exp(predict(model, new_data)))


mi <- ceteris_paribus(explainer_bst, observations = test_x[5,])
plot(profile_bst, selected_variables = "GrLivArea")




mi1 <- variable_response(explainer_bst,
                         variable = "GrLivArea",
                         type = "pdp")

mi <- variable_response(explainer_reg,
                        variable = "GrLivArea",
                        type = "pdp")

plot(mi, mi1)


plot(model_performance(explainer_bst))

mi <- variable_response(explainer_bst,
                        variable = "GrLivArea",
                        type = "pdp")

plot(mi)
imp <- variable_importance(explainer_bst, loss_function = loss_root_mean_square)

plot(imp)

single_variable(explainer = explainer_bst, 
                variable = "GrLivArea",
                type = "pdp") %>% 
  plot()



ggplot(
  data = train,
  aes(
    x = OverallQual,
    y = SalePrice
  )
) + 
  geom_jitter(alpha = 0.4, color = "steelblue", stroke = 0) 


new_house <- single_prediction(explainer_reg, observation = t(test_x[25,]))

new_house <- as_data_frame(new_house)

new_house <- as_tibble(new_house %>% arrange(desc(abs(contribution))))

new_house %>% 
  arrange(desc(abs("$contribution")))

new_house %>% 
  head() %>% 
  ggplot(aes(y = contribution,
             x = variable)) +
  geom_col() +
  coord_flip()


plot(new_house)


impo_reg <- variable_importance(explainer_reg, loss_function = loss_root_mean_square)
plot(impo_reg)



neighbours <- select_neighbours(houses_test,
                                observation = houses_test[25,],
                                n = 5)

profile_rf_neig  <- ceteris_paribus(explainer_bst,  
                                    observations = select(neighbours, -Id, -SalePrice) %>% 
                                      as.matrix(), 
                                    y = neighbours$SalePrice)
plot(profile_rf_neig)

plot(profile_rf_neig, 
     selected_variables = "GrLivArea", size_residuals = 2,
     color_residuals = "red", show_residuals = TRUE, show_observations = FALSE) 

# La regresión lineal es un modelo claramente explicativo
# ya que el significado del parámetro asociado a cada
# variable tiene un significado.
# Además, si estandarizamos los datos
# es decir, si a cada variable le restamos su media y
# dividimos entre la desviación típica,
# podemos obtener la importancia de cada variable.

reg_lineal_stand <- lm(SalePrice ~ GrLivArea + FullBath, data = as.data.frame(train))
summary(reg_lineal_stand)

train_stand <- train
train_stand[, "GrLivArea"] <- (train_stand[, "GrLivArea"] - mean(train_stand[, "GrLivArea"]))/sd(train_stand[, "GrLivArea"])
train_stand[, "FullBath"] <- (train_stand[, "FullBath"] - mean(train_stand[, "FullBath"]))/sd(train_stand[, "FullBath"])

reg_lineal_stand2 <- lm(SalePrice ~ GrLivArea + FullBath, data = as.data.frame(train_stand))
summary(reg_lineal_stand2)


# En el gradient boosting,
# no tenemos unos parámetros que nos permitan decir cómo
# contribuye cada una de las variables.
# Tenemos que conformarnos con la importancia
# relativa de cada variable:
imp <- xgb.importance(model = gradient_boost_log)
xgb.plot.importance(imp,top_n = 10)

# Hay paquetes dedicados a la interpretabilidad
# Por ejemplo, DALEX
# Fuente: https://pbiecek.github.io/DALEX/
install.packages("DALEX")
library(DALEX)

explainer_lm <- explain(reglineal_glmnet, data = test[, 1:261], y = test[, "SalePrice"])

vi_lm <- variable_importance(explainer_lm, loss_function = error)
plot(vi_lm)

int_prediccion <- single_prediction(explainer_lm, observation = t(as.matrix(test[1, 1:261])))
plot(int_prediccion)
breakDown:::print.broken(int_prediccion)
