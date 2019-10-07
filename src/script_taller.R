#' 
#' MSMK: TALLER DE MACHINE LEARNING
#' 
#' Pablo Hidalgo (pablohigar@gmail.com)
#' 

#' Los datos usados para este taller 
#' están descargados de la competición 
#' de Kaggle del siguiente enlace:
#' https://www.kaggle.com/c/house-prices-advanced-regression-techniques
#' 

#' En esta práctica vamos a aplicar varias técnicas para
#' predecir el precio de venta de una casa en función
#' de sus características: superficie, número de habitaciones, etcétera.


# DEPENDECIAS -------------------------------------------------------------

# Si es necesario, tienes que instalar el paquete tidyverse.
# Lo puedes hacer en la siguiente línea
# install.packages("tidyverse")
library(tidyverse)

# IMPORTAR DATOS ----------------------------------------------------------

train <- 

# UN VISTAZO DE LOS DATOS -------------------------------------------------

# Hemos visto que el machine learning se centra en los algoritmos.
# No obstante, en el "mundo real" uno nunca ejecuta un algoritmo
# sin antes haber entendido qué contienen los datos.

# El nombre de las variables del conjunto de datos
# se puede extraer con la función names()
names(train)

# Además, hay que asegurarse que los datos se han importado correctamente,
# es decir, que el tipo de cada variable es el adecuado. 
# Cuando hemos utilizado la función read_csv en la sección anterior,
# R busca internamente cuál es el tipo de cada variable leyendo 
# las primeras filas. A veces puede no hacerlo del todo correcto.
# 
# La función glimpse() nos puede ayudar a asegurarnos del tipo de las variables.
glimpse(train)

# En este caso, la variables se han importado correctamente.
# La información de qué contiene cada variable aparece 
# en data/raw/data_description.txt

# Una de las primeras ideas para predecir el precio de una vivienda (SalePrice)
# es utilizar la superficie la superficie (GrLivArea).


# En el conjunto de datos original, la superficie está expresada en
# pies cuadrados.
# Vamos a convertir esta variable a metros cuadrados para 
# tratar con una medida a la que estamos más acostumbrados.

train$GrLivArea <- train$GrLivArea/#¿?
summary(train$GrLivArea)


# Histograma de la superficie
ggplot(
  data = train,
  aes(
    x = 
  )
) + 
  geom_()

# Graficamos la relación entre el precio y la superficie
ggplot(
  data = train,
  aes(
    x = ,
    y = 
  )
) + 
  geom_() + 
  labs(
    title = "",
    caption = ""
  ) 



# El comportamiento de viviendas con más de 400 m2
# parece ser algo extraño.
# Habría que analizar con detenimiento estas viviendas.
# De momento, vamos a eliminar aquellas observaciones 
# con una superficie mayor de 400 m2.

train <- 
# Análogo a lo anterior
train <- 

# En un proyecto real (y más si es de data science), habría
# que hacer un repaso de las variables para detectar
# y corregir fallos en los datos igual que hemos hecho
# con la GrLivArea

# El paquete skimr es útil para empezar a detectar patrones

# install.packages("skimr")
skimr::skim(train)


# Depende del modelo que utilicemos,
# es admisible la existencia de 
# datos ausentes (missings).
# En este caso, vamos a imputar
# los valores ausentes por 0
train[] <- 0

# Nota (opcional):
# acabamos de imputar a 0 tanto los
# datos ausentes de variables numéricas
# como categóricas.
# Si quisiésemos imputar a 0 las variables
# numéricas y a "ausente" las variables categóricas,
# podríamos hacerlo así:
train <- mutate_if(train, 
                   is.character, 
                   function(x) ifelse(is.na(x), "ausente", x)
                   ) 

train <- mutate_if(train, 
                   is.numeric, 
                   function(x) ifelse(is.na(x), 0, x)
                   ) 
            

# Los modelos de machine learning, al final,
# no son más que operaciones aritméticas.
# Por lo tanto, no podemos tener variables
# expresadas como cadenas de caracteres.
# Existen varias estrategias para convertir
# una variable categórica a numérica.
# Vamos a utilizar una denominada
# ONE HOT ENCODING en la que, para cada categoría
# se genera una variable dummy 0,1.
# Por ejemplo, si tuviésemos una variable
# con valores ("rojo", "negro", "negro", "rojo")
# se generarían dos variables:
#   rojo: (1, 0, 0, 1)
#   negro: (0, 1, 1, 0)

# Esto lo conseguimos con la función model.matrix.
# Nota: la varible Id es simplemente un identificador,
# por eso se excluye a a partir de ahora.
?model.matrix
train <- model.matrix(, data = )

# Nota:
# en el ejemplo anterior de rojo y negro,
# sería suficiente con generar una de las dos
# variables ya que serían redundantes. 
# Es precisamente lo que hace model.matrix:
# cada variable categórica se desagrega en 
# n-1 categorías.

# DIVISIÓN EN TRAIN Y TEST ------------------------------------------------

# Necesitamos tener una idea del error cometido por nuestro modelo.
# Hay varias formas de hacer esto, algunas más sofisticadas que otras.
# Aquí simplemente vamos a dividir el conjunto de train en dos.
# Uno de ellos lo utilizaremos para entrenar el modelo y otro para
# validar los resultados.

# Es importante fijar la semilla para la partición aleatoria
# para poder tener resultados replicables.
set.seed(123)

id_train <- sample(, size = )

test <- train[-id_train,]
train <- train[id_train,]


# Generamos un data frame con la variable objetivo de test
# para poder comparar después con facilidad los resultados
# de los modelos.
test_error <- tibble(SalePrice = test[, "SalePrice"])

# ENTRENAMIENTO -----------------------------------------------------------

# REGRESIÓN LINEAL --------------------------------------------------------


# Comenzamos con un modelo básico, una regresión lineal.
# Este modelo tiene la ventaja de ser eficiente desde
# punto de vista computacional (entrena y predice rápidamente)
# y sencillo de explicar sus resultados.


# reg_lineal1 -------------------------------------------------------------

# Empezamos con una regresión lineal simple utilizando
# solamente la superficie de la casa para predecir
# el precio de venta
reg_lineal1 <- lm(, data = as.data.frame(train))

# PREGUNTA: 
# 1. ¿Qué significa el coeficiente de GrLivArea?
# 2. ¿Qué significa el coeficiente del intercept?

# Los datos de test los hemos dejado fuera del entrenamiento 
# para poder saber cómo se comportaría el modelo en unos datos nuevos.
test_error$predict_reglineal1 <- 

# En función del objetivo del proyecto,
# será más adecuado medir el error de una
# determinada manera.
# 
# En el caso de la competición de Kaggle,
# el error cometido por el modelo se mide
# con LA RAÍZ DEL ERROR CUADRÁTICO MEDIO
# tomando logaritmos
# Fuente: 
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques/overview/evaluation

# Construimos la función de error 
error <- function(real, pred) {
}

# Inicializamos un data frame en el que
# vamos a ir almacenando los errores 
# cometidos por cada modelo.
modelo_error <- tibble(
  modelo = "reglineal1",
  error = 
)

# Cuando se entrena un modelo,
# es muy importante asegurarse
# de sus propiedades y no solamente
# del error cometido.
# Es algo que se critica mucho 
# en el machine learning:
# a veces se reduce la utilidad
# de un modelo al error de predicción obtenido.

# Cada modelo matemático tiene una serie
# de hipótesis que se deben verificar.
# Sin embargo, algunas propiedades
# son comunes a todos los modelos.
# 
# El siguiente gráfico enfrenta el valor real
# con la predicción.
# En un modelo perfecto, los puntos deberían
# caer sobre la línea roja.

ggplot(data = test_error,
       aes(
        x = , 
        y = 
        )
       ) + 
  geom_point(color = "steelblue",
             alpha = 0.4) + 
  geom_abline() + 
  theme_minimal() + 
  labs(
    title =  "SalePrice real vs. predicción",
    subtitle = "En un modelo perfecto, los puntos deberían caer sobre la línea roja.",
    caption = "MSMK: taller de machine learning"
  )


# Otro gráfico importante es el
# gráfico de residuos: 
ggplot(data = test_error,
       aes(
         x = predict_reglineal1, 
         y = 
       )
) + 
  geom_point(color = "steelblue",
             alpha = 0.4) + 
  geom_abline(slope = , 
              intercept = , 
              color = "firebrick") + 
  theme_minimal() + 
  labs(
    title =  "SalePrice real vs. predicción",
    subtitle = "En un modelo perfecto, los puntos deberían caer sobre la línea roja.",
    caption = "MSMK: taller de machine learning"
  )



# reg_lineal2 -------------------------------------------------------------

# Es evidente que el precio de venta de una vivienda
# intervienen más factores que solamente su superficie.
# Vamos a predecir el precio en función de
# todas las variables que tenemos disponibles en el
# conjunto de datos

reg_lineal2 <- lm(, data = as.data.frame(train))

summary(reg_lineal2)

test_error$predict_reglineal2 <- predict(reg_lineal2, newdata = as.data.frame(test))

ggplot(data = test_error,
       aes(
         x = predict_reglineal2, 
         y = predict_reglineal2 - SalePrice
       )
) + 
  geom_point(color = "steelblue",
             alpha = 0.4) + 
  geom_abline(slope = 0, 
              intercept = 0, 
              color = "firebrick") + 
  theme_minimal() + 
  labs(
    title =  "SalePrice real vs. predicción",
    subtitle = "En un modelo perfecto, los puntos deberían caer sobre la línea roja.",
    caption = "MSMK: taller de machine learning"
  )


modelo_error <- bind_rows(modelo_error,
                          list(modelo = "reg_lineal2", 
                               error = error(test_error$SalePrice, test_error$predict_reglineal2))
                          )



# REGRESIÓN SELECCIONANDO VARIABLES ---------------------------------------

# Hay variables que pueden no ser relevantes para la predicción.
# La función lm coge todas las variables sin evaluar
# si son importantes en el modelo.

# Hay varios métodos par solucionar el problema.

# El paquete glmnet permite ajustar el modelo a la vez
# que selecciona las variables relevantes.

# install.packages("glmnet")
library(glmnet)
# La variable 262 es la variable objetivo, por eso se exlcuye
reglineal_glmnet <- glmnet(x = , y = )

test_error$predict_reglineal_glmnet <- predict(, newx = , s = 0.01)

modelo_error <- bind_rows(modelo_error,
                          list(modelo = "reglineal_glmnet", 
                               error = error(, )
                               )
                          )

# BOOSTING ----------------------------------------------------------------

# install.packages("xgboost")

library(xgboost)


gradient_boost <- xgboost(data = , 
                          label = ,
                          nrounds = 100
                          )

test_error$gradient_boosting <- predict()



modelo_error <- bind_rows(modelo_error,
                          list(modelo = , 
                               error = 
                               )
                          )


# RANDOM FOREST -----------------------------------------------------------
# install.packages("randomForest")

library(randomForest)

?randomForest
rf <- 

test_error$random_forest <- predict(rf, newdata = test[, 1:261])


modelo_error <- bind_rows(modelo_error,
                          list(modelo = "random_forest", 
                               error = error(test_error$SalePrice, test_error$random_forest)
                               )
                          )


# LOGARITMO ---------------------------------------------------------------

# Para encontrar un modelo mejor,
# se pueden seguir varias estrategias
# que se hacen, habitualmente, de forma conjunta.

# La estrategia más determinante es hacer una
# revisión exhaustiva de los datos para limpiar
# errores, agrupar valores, etcétera.

# Otra estrategia es la de ejecutar
# modelos de machine learning cambiando parámetros.
# Esto requiere cierta experiencia de configuraciones
# iniciales de los parámetros y luego ir haciendo ensayo error.

gradient_boost2 <- xgboost(data = train[, 1:261], 
                          label = train[,"SalePrice"],
                          nrounds = ,
                          params = list(
                            
                          )
                          )

test_error$gradient_boosting2 <- predict(gradient_boost2, test[, 1:261])

modelo_error <- bind_rows(modelo_error,
                          list(modelo = "gradient_boosting2", 
                               error = error(test_error$SalePrice, test_error$gradient_boosting2)
                               )
                          )


# Otra estrategia es hacer transformaciones sobre los datos
# que permitan recoger mejor la tendencia.

# install.packages("devtools")
# devtools::install_github("thomasp85/patchwork")
library(patchwork)

p1 <- ggplot(
  data = as.data.frame(train),
  aes(
    x = GrLivArea,
    y = SalePrice
  )
) + 
  geom_point(alpha = 0.4, color = "steelblue") + 
  geom_smooth(method = "lm", color = "firebrick") +
  # geom_smooth(color = "firebrick") +
  scale_x_continuous(labels = function(x) paste(x, "m²")) +
  scale_y_continuous(labels = function(x) paste(x/1000, "k")) +
  theme_minimal() + 
  labs(
    title = "Superficie de la casa frente al precio de venta (original)",
    caption = "MSMK: taller de machine learning"
  ) 

p2 <- ggplot(
  data = as.data.frame(train),
  aes(
    x = GrLivArea,
    y = log(SalePrice)
  )
) + 
  geom_point(alpha = 0.4, color = "steelblue") + 
  geom_smooth(method = "lm", color = "firebrick") +
  # geom_smooth(color = "firebrick") +
  scale_x_continuous(labels = function(x) paste(x, "m²")) +
  scale_y_continuous(labels = function(x) paste(x/1000, "k")) +
  theme_minimal() + 
  labs(
    title = "Superficie de la casa frente al precio de venta (tomando logaritmo)",
    caption = "MSMK: taller de machine learning"
  ) 

p1 + p2


# Repitiendo los modelos tomando logaritmos

reglineal_glmnet_log <- glmnet(x = , y = )

test_error$predict_reglineal_glmnet_log <- 

modelo_error <- bind_rows(modelo_error,
                          list(modelo = "predict_reglineal_glmnet_log", 
                               error = error(test_error$SalePrice, test_error$predict_reglineal_glmnet_log)
                          )
                          )


gradient_boost_log <- xgboost(data = , 
                           label = ,
                           nrounds = ,
                           params = list(
                             
                           )
)

test_error$gradient_boosting_log <- exp(predict(gradient_boost_log, test[, 1:261]))

modelo_error <- bind_rows(modelo_error,
                          list(modelo = "gradient_boosting_log", 
                               error = error(test_error$SalePrice, test_error$gradient_boosting_log)
                          )
)


# EXPLICATIVIDAD ----------------------------------------------------------

# Algo en lo que se está trabajando activamente en el mundo
# del machine learning es en tratar de hacer que los 
# modelos no sean cajas negras.

# La regresión lineal es un modelo claramente explicativo
# ya que el significado del parámetro asociado a cada
# variable tiene un significado.
# Además, si estandarizamos los datos
# es decir, si a cada variable le restamos su media y
# dividimos entre la desviación típica,
# podemos obtener la importancia de cada variable.

reg_lineal_stand <- lm(SalePrice ~ , data = as.data.frame(train))
summary(reg_lineal_stand)

train_stand <- train
train_stand[, "GrLivArea"] <- ( - mean())/sd()
train_stand[, "FullBath"] <- 

reg_lineal_stand2 <- lm(, data = )
summary(reg_lineal_stand2)


# En el gradient boosting,
# no tenemos unos parámetros que nos permitan decir cómo
# contribuye cada una de las variables.
# Tenemos que conformarnos con la importancia
# relativa de cada variable:


# Hay paquetes dedicados a la interpretabilidad
# Por ejemplo, DALEX
# Fuente: https://pbiecek.github.io/DALEX/
install.packages("DALEX")
library(DALEX)

explainer_lm <- explain(reglineal_glmnet, data = test[, 1:261], y = test[, "SalePrice"])


