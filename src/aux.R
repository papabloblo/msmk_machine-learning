
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
