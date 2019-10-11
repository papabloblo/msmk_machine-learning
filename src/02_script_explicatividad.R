#' Nota:
#'   Para poder ejecutar este script, necesitas haber ejecutado antes
#'   el script 01_script_modelos.R


#' Vamos a intentar explicar cuáles son los resultados de los modelos que
#' hemos ajustado. 
#' Como ejemplo, vamos a utilizar reglineal_glmnet_log y 
#' gradient_boosting.


residuos <- 
  test_prediction %>% 
  transmute(
    SalePrice,
    
    reglineal_glmnet_log = predict_reglineal_glmnet_log,
    res_reglineal = SalePrice - reglineal_glmnet_log,
    
    gradient_boosting,
    res_gradient_boosting =  SalePrice - gradient_boosting
    
  )



residuos %>% 
  ggplot(
    aes(x = reglineal_glmnet_log,
        y = SalePrice)
  ) + 
  geom_point(color = "steelblue", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "firebrick") +
  labs(
    title = "Predicho frente a real",
    x = "Precio predicho (en miles de dólares)",
    y = "Precio real (en miles de dólares)"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = function(x) paste(x/1000, "k")) +
  scale_y_continuous(labels = function(x) paste(x/1000, "k")) 


residuos %>% 
  ggplot(
    aes(x = reglineal_glmnet_log,
        y = res_reglineal)
  ) + 
  geom_point(color = "steelblue", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "firebrick") +
  labs(
    title = "Residuos de la regresión lineal",
    x = "Precio predicho (en miles de dólares)",
    y = "residuos"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = function(x) paste(x/1000, "k")) 



residuos %>% 
  ggplot(
    aes(x = gradient_boosting,
        y = res_gradient_boosting)
  ) + 
  geom_point(color = "steelblue", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "firebrick") +
  labs(
    title = "Residuos de boosting",
    x = "Precio predicho (en miles de dólares)",
    y = "residuos"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = function(x) paste(x/1000, "k")) 


reglm <- residuos %>% 
  transmute(
    SalePrice,
    prediccion = reglineal_glmnet_log,
    res = res_reglineal,
    modelo = "Regresión lineal"
  )

bst <- residuos %>% 
  transmute(
    SalePrice, 
    prediccion = gradient_boosting,
    res = res_gradient_boosting,
    modelo = "Boosting"
  )

res <- reglm %>% 
  bind_rows(bst)

res %>% 
  ggplot() +
  geom_point(
    aes(x = prediccion, 
        y = res, 
        color = modelo
    ),
    alpha = 0.5
  ) +
  geom_hline(yintercept = 0, color = "firebrick") +
  labs(
    title = "Comparativa de residuos",
    x = "Predicción",
    x = "Residuos"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = function(x) paste(x/1000, "k"))  +
  scale_y_continuous(labels = function(x) paste(x/1000, "k")) 



res %>% 
  ggplot() +
  geom_point(
    aes(x = prediccion, 
        y = res, 
        color = modelo
    ),
    alpha = 0.5
  ) +
  geom_hline(yintercept = 0, color = "firebrick") +
  facet_grid(~modelo) +
  labs(
    title = "Comparativa de residuos",
    x = "Predicción",
    x = "Residuos"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = function(x) paste(x/1000, "k"))  +
  scale_y_continuous(labels = function(x) paste(x/1000, "k")) 





res %>% 
  ggplot() +
  geom_density(
    aes(x = res, 
        fill = modelo,
        color = modelo
    ),
    alpha = 0.5
  ) +
  geom_vline(xintercept = 0, color = "firebrick") +
  labs(
    title = "Comparativa de residuos",
    x = "Residuos"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = function(x) paste(x/1000, "k")) 



res %>% 
  ggplot() +
  geom_point(
    aes(x = prediccion, 
        y = SalePrice, 
        color = modelo
    ),
    alpha = 0.5
  ) +
  geom_abline(intercept = 0, slope = 1,  color = "firebrick") +
  # facet_grid(~modelo) +
  labs(
    title = "Predicho vs. real",
    x = "Predicción",
    y = "Real"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = function(x) paste(x/1000, "k"))  +
  scale_y_continuous(labels = function(x) paste(x/1000, "k")) 



res %>% 
  ggplot() +
  geom_point(
    aes(x = SalePrice, 
        y = res, 
        color = modelo
    ),
    alpha = 0.5
  ) +
  geom_hline(yintercept = 0,color = "firebrick") +
  # facet_grid(~modelo) +
  labs(
    title = "Real vs. residuos",
    x = "Real",
    y = "Residuos"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = function(x) paste(x/1000, "k"))  +
  scale_y_continuous(labels = function(x) paste(x/1000, "k")) 

# EXPLICATIVIDAD ----------------------------------------------------------

# Referencia: https://github.com/ModelOriented/DALEX
library(DALEX)


# IMPORTANCIA DE VARIABLES ------------------------------------------------

explainer_bst <- explain(gradient_boost, 
                         data = test_x,
                         y = houses_test$SalePrice
                         )

explainer_reg <- explain(reglineal_glmnet_log, 
                         data = train_x,
                         y = houses_training$SalePrice,
                         predict_function = function(model, new_data) exp(predict(model, new_data))
                         )


imp_bst <- variable_importance(explainer_bst, 
                               loss_function = loss_root_mean_square
                               )
plot(imp_bst)

imp_reg <- variable_importance(explainer_reg, 
                               loss_function = loss_root_mean_square
                               )
plot(imp_reg)


# PDP ---------------------------------------------------------------------


grlivarea_reg <- variable_response(explainer_reg,
                                   variable = "GrLivArea",
                                   type = "pdp"
                                   )


grlivarea_bst <- variable_response(explainer_bst,
                                  variable = "GrLivArea",
                                  type = "pdp"
                                  )

plot(grlivarea_reg, grlivarea_bst)



qual_reg <- variable_response(explainer_reg,
                                   variable = "OverallQual",
                                   type = "pdp"
)


qual_bst <- variable_response(explainer_bst,
                                   variable = "OverallQual",
                                   type = "pdp"
)

plot(qual_reg, qual_bst)






new_house <- single_prediction(explainer_reg, observation = t(test_x[25,]))

saveRDS(new_house, "models/new_house.RDS")
new_house <- readRDS("models/new_house.RDS")

new_house %>% arrange(-abs(contribution)) %>% as_tibble()
