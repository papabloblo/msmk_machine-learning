#' EXPLICATIVIDAD DE MODELOS
#' Taller ML
#' Pablo Hidalgo (pablohigar@gmail.com)
#' 

#' En este ejercicio vamos a comparar entre sí dos de los modelos que 
#' hemos ajustado:
#'   - glmnet utilizando logaritmo
#'   - Gradient boosting
#'   
#'  NOTA:
#'   Para poder ejecutar este script, necesitas haber ejecutado antes
#'   el script 01_script_modelos.R
#'   Puedes abrir el script y ejecutarlo entero o hacerlo con la siguiente
#'   sentencia:
source("src/01_script_modelos.R")


# Primera inspección ------------------------------------------------------

#' Para comodidad de los cálculos posteriores, creamos un data.frame con
#' el precio real, la predicción de cada modelo y su residuo (en test)

residuos <- test_prediction %>% 
  transmute(
    SalePrice,
    
    reglineal_glmnet_log = predict_reglineal_glmnet_log,
    res_reglineal = SalePrice - reglineal_glmnet_log,
    
    gradient_boosting,
    res_gradient_boosting =  SalePrice - gradient_boosting
  )

residuos$Id <- houses_test$Id

reglm <- residuos %>% 
  transmute(
    Id,
    SalePrice,
    prediccion = reglineal_glmnet_log,
    res = res_reglineal,
    modelo = "Regresión lineal"
  )

bst <- residuos %>% 
  transmute(
    Id,
    SalePrice, 
    prediccion = gradient_boosting,
    res = res_gradient_boosting,
    modelo = "Boosting"
  )

residuos <- reglm %>% 
  bind_rows(bst)



#' Un gráfico importante es comparar la predicción frente al valor real.
#' Empezamos viendo el gráfico para la regresión:

residuos %>% 
  filter(modelo == "Regresión lineal") %>% 
  ggplot(
    aes(x = prediccion,
        y = SalePrice)
  ) + 
  geom_point(color = "steelblue", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1) +
  labs(
    title = "Predicho frente a valor real",
    subtitle = "Si la predicción fuese perfecta, los puntos debería caer sobre la línea",
    x = "Precio predicho (en miles de dólares)",
    y = "Precio real (en miles de dólares)",
    caption = "MSMK: taller de ML"
  ) +
  scale_x_continuous(labels = function(x) paste(x/1000, "k")) +
  scale_y_continuous(labels = function(x) paste(x/1000, "k")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, 
                              face = "bold",
                              family = "sans",vjust = 0.5,margin = ggplot2::margin(b = 5)
                              ),
    plot.subtitle = element_text(margin = ggplot2::margin(b = 30))
    )
  

#' Lo interesante es comparar los dos modelos.
#' 
residuos %>% 
  ggplot(
    aes(x = prediccion,
        y = SalePrice,
        color = modelo)
  ) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1) +
  labs(
    title = "Predicho frente a valor real",
    subtitle = "Si la predicción fuese perfecta, los puntos debería caer sobre la línea",
    x = "Precio predicho (en miles de dólares)",
    y = "Precio real (en miles de dólares)",
    caption = "MSMK: taller de ML",
    color = ""
  ) +
  scale_x_continuous(labels = function(x) paste(x/1000, "k")) +
  scale_y_continuous(labels = function(x) paste(x/1000, "k")) +
  scale_color_manual(values = c("Boosting" = "firebrick", "Regresión lineal" = "steelblue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, 
                              face = "bold",
                              family = "sans",vjust = 0.5,margin = ggplot2::margin(b = 5)
    ),
    plot.subtitle = element_text(margin = ggplot2::margin(b = 30)),
    legend.position = "top"
  )


#' Ya vimos el gráfico de residuos por separado. Vamos a verlos superpuestos

residuos %>% 
  ggplot(
    aes(y = res)
  ) + 
  geom_hline(yintercept = 0,  color = "grey2") +
  geom_point(
    aes(x = prediccion, colour = modelo),
    alpha = 0.5
  ) +
  labs(
    title = "Gráfico de residuos",
    subtitle = "Si la predicción fuese perfecta, los puntos debería caer sobre la línea horizontal",
    x = "Precio predicho (en miles de dólares)",
    y = "Residuos (real - predicción) en miles de dólares",
    caption = "MSMK: taller de ML",
    color = ""
  ) +
  scale_x_continuous(labels = function(x) paste(x/1000, "k")) +
  scale_y_continuous(labels = function(x) paste(x/1000, "k")) +
  scale_color_manual(values = c("Boosting" = "firebrick", "Regresión lineal" = "steelblue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, 
                              face = "bold",
                              family = "sans",vjust = 0.5,margin = ggplot2::margin(b = 5)
    ),
    legend.position = "top"
  ) 

#' Un gráfico de residuos un poco especial


test_prediction %>% 
  transmute(
    SalePrice,
    
    reglineal_glmnet_log = predict_reglineal_glmnet_log,
    res_reglineal = SalePrice - reglineal_glmnet_log,
    
    gradient_boosting,
    res_gradient_boosting =  SalePrice - gradient_boosting
  ) %>% 
  ggplot(
  ) + 
  geom_hline(yintercept = 0,  color = "grey2") +
  geom_segment(
    aes(
      x = gradient_boosting,
      xend = reglineal_glmnet_log,
      
      y = res_gradient_boosting,
      yend = res_reglineal
    ),
    alpha = 0.5,
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  labs(
    title = "Gráfico de residuos",
    subtitle = "La flecha va desde la predicción del Gradient Boosting hasta la regresión lineal",
    x = "Precio predicho (en miles de dólares)",
    y = "Residuos (real - predicción) en miles de dólares",
    caption = "MSMK: taller de ML",
    color = ""
  ) +
  scale_x_continuous(labels = function(x) paste(x/1000, "k")) +
  scale_y_continuous(labels = function(x) paste(x/1000, "k")) +
  scale_color_manual(values = c("Boosting" = "firebrick", "Regresión lineal" = "steelblue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, 
                              face = "bold",
                              family = "sans",vjust = 0.5,margin = ggplot2::margin(b = 5)
    ),
    legend.position = "top"
  ) 


#' En un proyecto "real" deberíamos comprobar las observaciones con los
#' residuos más grandes para buscar cosas a mejorar.
residuos %>% 
  arrange(desc(abs(res)))

houses_test %>% 
  filter(Id == 1170) %>% 
  glimpse()


#' Distribución de los residuos

residuos %>% 
  ggplot(
    aes(x = res)
  ) +
  geom_vline(xintercept = 0,  color = "grey2") +
  geom_density(
    aes(fill = modelo, color = modelo),
    alpha = 0.5
  ) +
  labs(
    title = "Distribución de residuos",
    x = "Residuos (real - predicción) en miles de dólares",
    y = "",
    caption = "MSMK: taller de ML",
    fill = ""
  ) +
  guides(color = FALSE) +
  scale_x_continuous(labels = function(x) paste(x/1000, "k")) +
  scale_y_continuous(labels = function(x) paste(x/1000, "k")) +
  scale_color_manual(values = c("Boosting" = "firebrick", "Regresión lineal" = "steelblue")) +
  scale_fill_manual(values = c("Boosting" = "firebrick", "Regresión lineal" = "steelblue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, 
                              face = "bold",
                              family = "sans",vjust = 0.5,margin = ggplot2::margin(b = 5)
    ),
    legend.position = "top"
  ) 


# EXPLICATIVIDAD ----------------------------------------------------------

# Referencia: https://github.com/ModelOriented/DALEX
library(DALEX)
library(ingredients)

#' Para poder obtener elementos que nos permitan explicar los modelos,
#' necesitamos utilizar primero la función explain.

explainer_bst <- explain(gradient_boost, 
                         data = test_x,
                         y = houses_test$SalePrice
                         )

explainer_reg <- explain(reglineal_glmnet_log, 
                         data = train_x,
                         y = houses_training$SalePrice,
                         predict_function = function(model, new_data) exp(predict(model, new_data))
                         )


# IMPORTANCIA DE VARIABLES ------------------------------------------------

#' Un paso importante tanto para comenzar a explicar el modelo como
#' para detectar posibles errores en él es el de calcular
#' la importancia de las variables.

imp_bst <- ingredients::feature_importance(explainer_bst, 
                               loss_function = loss_root_mean_square
                               )

imp_reg <- ingredients::feature_importance(explainer_reg, 
                               loss_function = loss_root_mean_square
                              )

p1 <- imp_bst %>% 
  filter(variable != "_baseline_") %>% 
  top_n(15, wt = dropout_loss) %>% 
  ggplot(aes(x = fct_reorder(variable, dropout_loss), y = dropout_loss)) +
  geom_col(fill = "firebrick") +
  labs(
    title = "15 variables más importantes\n para el modelo de gradient boosting",    
    x = "",
    y = "",
    caption = "MSMK: taller de ML"
  ) +
  coord_flip() +
  theme_minimal()

p1

p2 <- imp_reg %>% 
  filter(variable != "_baseline_") %>% 
  top_n(15, wt = dropout_loss) %>% 
  ggplot(aes(x = fct_reorder(variable, dropout_loss), y = dropout_loss)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "15 variables más importantes\n para el modelo de glmnet",    
    x = "",
    y = "",
    caption = "MSMK: taller de ML"
  ) +
  coord_flip() +
  theme_minimal()
p2

#' Opcional: varios gráficos en uno
# install.packages("devtools")
devtools::install_github("thomasp85/patchwork")

library(patchwork)

p1 + p2 


# PDP ---------------------------------------------------------------------


pdp_bst <- partial_dependency(explainer_bst,
                                    variables = c("GrLivArea", "OverallQual")
                                  )



as_tibble(pdp_bst) %>% 
  ggplot(aes(x = `_x_`, y = `_yhat_`)) +
  geom_line() +
  facet_wrap(~`_vname_`, scales = "free") +
  labs(x = "", y = "") +
  theme_minimal()


selected_houses <- select_sample(test_x, n = 100)
cp_bst <- ceteris_paribus(explainer_bst, selected_houses, 
                          variables = c("GrLivArea", "OverallQual"))

plot(cp_bst)


# Observación -----------------------------------------------------------------
library(iBreakDown)

new_house <- break_down(explainer_bst, t(test_x[25,]))

plot(new_house)

