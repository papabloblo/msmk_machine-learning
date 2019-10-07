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
# Lo puedes hacer en la siguiente línea:
# install.packages("tidyverse")
library(tidyverse)

# IMPORTAR DATOS ----------------------------------------------------------

train <- read_csv("data/raw/train.csv")

# UN VISTAZO A LOS DATOS -------------------------------------------------

# Hemos visto que el machine learning se centra en los algoritmos.
# No obstante, en el "mundo real" uno nunca ejecuta un algoritmo
# sin antes haber entendido qué contienen los datos 
# y, si es necesario, tratarlos.

# El nombre de las variables del conjunto de datos
# se puede extraer con la función names()
names(train)

# Además, hay que asegurarse de que los datos se han importado correctamente,
# es decir, que el tipo de cada variable es el adecuado. 
# Cuando hemos utilizado la función read_csv en la sección anterior,
# R busca internamente cuál es el tipo adecuado para cada variable leyendo 
# las primeras filas. A veces puede no hacerlo del todo correcto.
# 
# La función glimpse() nos puede ayudar a asegurarnos del tipo de las variables.
glimpse(train)

# En este caso, la variables se han importado correctamente.

# NOTA: La información de qué contiene cada variable aparece 
# en data/raw/data_description.txt

# La variable que queremos predecir es el PRECIO DE LA VIVIENDA (SalePrice).

# SUPERFICIE DE LA VIVIENDA
# Una de las primeras ideas para predecir el precio de una vivienda (SalePrice)
# es utilizar la superficie (GrLivArea).
summary(train$GrLivArea)

# La media de la superficie es 1515, un valor "extraño".
# Si miramos la información en data/raw/data_description.txt tenemos:
#     GrLivArea: Above grade (ground) living area square feet
# 
# La superficie está expresada en pies cuadrados.
# Vamos a convertir esta variable a metros cuadrados para 
# tratar con una medida a la que estamos más acostumbrados, algo 
# que es imporante para poder entender los resultados del modelo
# y poder detectar fallos en él.

# conversión a m^2
train$GrLivArea <- train$GrLivArea/10.764
summary(train$GrLivArea)

# Es importante conocer la estructura de la variable.
# Una forma sencilla es la de representarla gráficamente.

# Histograma de la superficie
ggplot(
  data = train,
  aes(
    x = GrLivArea
  )
) + 
  geom_histogram() + 
  theme_minimal() + 
  labs(
    title = "Histograma de la superficie de la casa en metros cuadrados",
    subtitle = "Variable GrLivArea",
    caption = "MSMK: taller de machine learning"
  ) 


ggplot(
  data = train,
  aes(x = GrLivArea)
  ) + 
  geom_histogram() + 
  geom_vline(
    aes(xintercept = mean(GrLivArea)), 
    color = "steelblue",
    size = 2
    ) +
  theme_minimal() + 
  labs(
    title = "Histograma de la superficie de la casa en metros cuadrados",
    subtitle = "Variable GrLivArea",
    caption = "MSMK: taller de machine learning"
  ) 

# Graficamos la relación entre el precio y la superficie
ggplot(
  data = train,
  aes(
    x = GrLivArea,
    y = SalePrice
    )
  ) + 
  geom_point(alpha = 0.4, color = "steelblue", stroke = 0) + 
  # geom_smooth(method = "lm", color = "steelblue") +
  geom_smooth(color = "firebrick") +
  scale_x_continuous(labels = function(x) paste(x, "m²")) +
  scale_y_continuous(labels = function(x) paste(x/1000, "k")) +
  theme_minimal() + 
  labs(
    title = "Superficie de la casa (GrLivArea) frente al precio de venta (SalePrice)",
    caption = "MSMK: taller de machine learning",
    x = "Superficie de la vivienda",
    y = "Precio de venta (en miles de euros)"
  ) 

# Algunas notas:
# Por convención, cuando hacemos un gráfico, en el eje y se sitúa la variable
# que queramos predecir y en el eje x la información para predecirla.
# En este caso, queremos predecir el precio (eje y) a partir de 
# la superficie (eje x).

# ¿Qué podemos decir después de ver el gráfico?
# El comportamiento de viviendas con más de 400 m2
# parece ser algo extraño.
# Habría que analizar con detenimiento estas viviendas.
# Depende del modelo que vayamos a utilizar, que existan estos valores
# anómalos puede comprometer la eficacia del modelo.
# Para simplificar, vamos a eliminar aquellas observaciones 
# con una superficie mayor de 400 m2.


train <- train[train$GrLivArea < 400,]
# Análogo a lo anterior
# train <- filter(train, GrLivArea < 400)

# Otra cosa que podemos apreciar es que el gráfico tiene "forma de embudo".
# En términos matemáticos se dice que la varianza del precio no es constante
# en función de la superficie (esto se llama heterocedasticidad).
# Igual que hemos dicho antes, esto puede ser más o menos relevante en
# función del modelo que utilicemos

# En un proyecto real, habría
# que hacer un repaso de las variables para detectar
# y corregir fallos en los datos igual que hemos hecho
# con la variable GrLivArea

# El paquete skimr es útil para empezar a detectar patrones
# a alto nivel.

# Si es necesario: install.packages("skimr")
skimr::skim(train)

# Pregunta: ¿Por qué la variable Id tiene ese histograma?




# ALGUNAS CORRECCIONES ----------------------------------------------------

# Si representamos el año de construcción de la casa (YearBuilt) y 
# el año de construcción del garaje (GarageYrBlt)

ggplot(
  data = train,
  aes(
    x = GarageYrBlt,
    y = YearBuilt
  )
) + 
  geom_point(alpha = 0.4, color = "steelblue", stroke = 0) +
  theme_minimal() 

# Hay observaciones en las que el año de construcción del garaje
# es anterior al año de construcción de la casa.
# ¿Esto puede ser? Si trabjásemos para una empresa, habría preguntarles
# si este comportamiento es posible o es un error.
# Vamos a suponer que es un error y vamos a tratar estos datos de forma
# que, si el año de construcción del garaje es anterior,
# pondremos el año de construcción de la casa.
train <- train %>% 
  mutate(
    GarageYrBlt = ifelse(GarageYrBlt < YearBuilt, 
                         YearBuilt, 
                         GarageYrBlt
                         )
    )

# Si vuelves a ejecutar el gráficos anterior, verás que se han solucionado
# los problemas.



# Depende del modelo que utilicemos,
# es admisible o no la existencia de 
# datos ausentes (missings).
# Si nos fijamos en el resultado de de la función skim anterior,
# por ejemplo, la variable LotFrontage tiene 259 valores ausentes.
# Existen diversos métodos para imputar datos ausentes. 
# En un primer momento, antes de recurrir a ningún método de imputación
# estadística, hay que reflexionar sobre la naturaleza de los datos
# y si los missing pueden tener un significado y ser imputados conforme
# a una lógica de negocio.

# En este caso, los missing puede ser un indicador de que la casa
# carece de esa característica y vamos a optar por imputar
# los valores ausentes por 0
train[is.na(train)] <- 0

# Nota 1 (opcional):
# acabamos de imputar a 0 tanto los
# datos ausentes de variables numéricas
# como categóricas.
# Si quisiésemos imputar a 0 las variables
# numéricas y a "ausente" las variables categóricas,
# podríamos hacerlo así:

# train <- mutate_if(train, 
#                    is.character, 
#                    function(x) ifelse(is.na(x), "ausente", x)
#                    ) 
# 
# train <- mutate_if(train, 
#                    is.numeric, 
#                    function(x) ifelse(is.na(x), 0, x)
#                    ) 
#             

# Nota 2 (opcional):
# hemos imputado sustituyendo los missings por 0.
# Antes de imputar, podríamos haber generado una variable nueva 
# por cada variable con datos ausentes con 1 para las obervaciones con
# dato ausente y 0 las que no.



# MODELIZACIÓN ------------------------------------------------------------

# Lo bueno de R (o malo, según se quiera mirar) es que existen muchas formas
# de hacer lo mismo.
# Para el ajuste de modelos de ML vamos a utilizar el paquete tidymodels
# Referencias: 
# https://github.com/tidymodels/tidymodels
# https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/

# install.packages("tidymodels")
library(tidymodels)

# DIVISIÓN EN TRAIN Y TEST ------------------------------------------------

# Necesitamos tener una idea del error cometido por nuestro modelo.
# Hay varias formas de hacer esto, algunas más sofisticadas que otras.
# Aquí simplemente vamos a dividir el conjunto de train en dos.
# Uno de ellos lo utilizaremos para entrenar el modelo y otro para
# validar los resultados.

# Es importante fijar la semilla para la partición aleatoria
# y poder tener resultados replicables.
set.seed(123)

# El 75% de los datos se utilizarán para entrenar el modelo
# y el 25% restante para validar los resultados.

# PREGUNTA: 
# ¿por qué hacer una partición aleatorio y no según vienen ordenados los datos?

train_split <- initial_split(train, prop = .75) 
train_split

# Conjunto de datos de entrenamiento
train_split %>% 
  training()

# Conjunto de datos de test
train_split %>% 
  testing()


# La siguiente sentencia crea una "receta" de cómo se tienen
# que preparar los datos para nuestro modelo.
train_recipe <- 
  # conjunto de datos con el que vamos a entrenar
  training(train_split) %>%
  # se define cuál es la variable que se quiere predecir (target)
  # Nota: la varible Id es simplemente un identificador,
  # por eso se excluye a partir de ahora.
  recipe(SalePrice ~.-Id) %>% 
  # para evitar redundancia, se eliminan variables con una correlación
  # superior a 0.9 (en valor absoluto)
  step_corr(all_predictors(), -all_nominal()) %>%
  
  # para cada variable categórica se generan tantas variables dummys (0/1)
  # como categorías tenga. (Ver nota posterior)
  step_dummy(all_predictors(), -all_numeric())


# aplicamos la receta sopre los datos
train_recipe <- prep(train_recipe)

# si queremos obtener el data frame del conjunto de datos:
houses_training <- juice(train_recipe)

# podemos aplicar la "receta" a cualquier conjunto de datos con una
# estructura igual. En concreto, nosotros querremos aplicar la receta
# sobre nuestro conjunto de datos de test:
houses_test <- bake(train_recipe, new_data = testing(train_split))

# NOTA:
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
# con valores ("chalet", "piso", "piso", "chalet")
# se generarían dos variables:
#   chalet: (1, 0, 0, 1)
#   piso: (0, 1, 1, 0)

# Esto lo conseguimos con la función step_dummy.

# Nota:
# en el ejemplo anterior de chalet y piso,
# sería suficiente con generar una de las dos
# variables ya que serían redundantes. 
# Es precisamente lo que hace model.matrix:
# cada variable categórica se desagrega en 
# n-1 categorías.


# IMPORTANTE:
# En un entorno real, queremos tener ya precalculado la "receta" para que
# se ajuste rápidamente sobre nuevos datos sin tener que volver a calcularla.
# En ese caso, debemos guardar en el disco duro el objeto de la receta:
saveRDS(train_recipe, "models/train_recipe.RDS")

# Si necesitamos cargar el objeto:
train_recipe <- readRDS("models/train_recipe.RDS")


# ENTRENAMIENTO -----------------------------------------------------------

# REGRESIÓN LINEAL --------------------------------------------------------


# Comenzamos con un modelo básico: una regresión lineal.
# Este modelo tiene la ventaja de ser eficiente desde
# punto de vista computacional (entrena y predice rápidamente)
# y sencillo de explicar sus resultados.


# reg_lineal1 -------------------------------------------------------------

# Empezamos con una regresión lineal simple utilizando
# solamente la superficie de la casa para predecir
# el precio de venta
reg_lineal1 <- lm(SalePrice ~ GrLivArea, data = houses_training)

summary(reg_lineal1)

# PREGUNTA: 
# 1. ¿Qué significa el coeficiente de GrLivArea?
# 2. ¿Qué significa el coeficiente del intercept?

# Vamos a representar la recta de regresión:
ggplot(
  data = houses_training,
  aes(
    x = GrLivArea,
    y = SalePrice
  )
) + 
  geom_point(alpha = 0.4, color = "steelblue", stroke = 0) + 
  geom_abline(
    intercept = reg_lineal1$coefficients[1],
    slope = reg_lineal1$coefficients[2], 
    color = "firebrick",
    size = 2,
    alpha = 0.8
    ) +
  scale_x_continuous(labels = function(x) paste(x, "m²")) +
  scale_y_continuous(labels = function(x) paste(x/1000, "k")) +
  theme_minimal() + 
  labs(
    title = "Superficie de la casa (GrLivArea) frente al precio de venta (SalePrice)",
    caption = "MSMK: taller de machine learning",
    x = "Superficie de la vivienda",
    y = "Precio de venta (en miles de euros)"
  ) 



# Algo muy importante son los residuos. Si el modelo fuese bueno,
# no deberíamos intuir ningún patrón en el gráfico de residuos.
ggplot(
  data = houses_training,
  aes(
    x = reg_lineal1$fitted.values,
    y = reg_lineal1$residuals
  )
) + 
  geom_point(alpha = 0.4, color = "steelblue", stroke = 0) + 
  scale_x_continuous(labels = function(x) paste(x, "m²")) +
  theme_minimal() + 
  labs(
    title = "Residuos de la regresión lineal",
    caption = "MSMK: taller de machine learning",
    x = "Superficie de la vivienda",
    y = "Residuo (predicción - precio real)"
  ) 

# ¿Qué puedes decir del gráfico de residuos?


# Los datos de test los hemos dejado fuera del entrenamiento 
# para poder saber cómo se comportaría el modelo en unos datos nuevos.
test_prediction <- tibble(
  SalePrice = houses_test$SalePrice,
  predict_reglineal1 = predict(reg_lineal1, newdata = houses_test)  
)




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
  return(
    sqrt(
      mean(
        (real - pred)^2,
        na.rm = TRUE
        )
      )
    )
}

# Inicializamos un data frame en el que
# vamos a ir almacenando los errores 
# cometidos por cada modelo.
modelo_error <- tibble(
  modelo = "reglineal1",
  error = error(test_prediction$SalePrice, test_prediction$predict_reglineal1)
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


# reg_lineal2 -------------------------------------------------------------

# Es evidente que el precio de venta de una vivienda
# intervienen más factores que solamente su superficie.
# Vamos a predecir el precio en función de
# todas las variables que tenemos disponibles en el
# conjunto de datos

reg_lineal2 <- lm(SalePrice~.-Id, data = houses_training)

summary(reg_lineal2)

test_prediction$predict_reglineal2 <- predict(reg_lineal2, newdata = houses_test)

ggplot(
  data = houses_training,
  aes(
    x = reg_lineal2$fitted.values,
    y = reg_lineal2$residuals
  )
) + 
  geom_point(alpha = 0.4, color = "steelblue", stroke = 0) + 
  scale_x_continuous(labels = function(x) paste(x, "m²")) +
  theme_minimal() + 
  labs(
    title = "Residuos de la regresión lineal",
    caption = "MSMK: taller de machine learning",
    x = "Superficie de la vivienda",
    y = "Residuo (predicción - precio real)"
  ) 



modelo_error <- bind_rows(modelo_error,
                          list(modelo = "reg_lineal2", 
                               error = error(test_prediction$SalePrice, test_prediction$predict_reglineal2))
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

train_x <- select(houses_training, -Id, -SalePrice) %>% 
  as.matrix()

reglineal_glmnet <- glmnet(x = train_x, y = houses_training$SalePrice)


test_x <- select(houses_test, -Id, -SalePrice) %>% 
  as.matrix()
test_x[is.na(test_x)] <- 0

test_prediction$predict_reglineal_glmnet <- predict(reglineal_glmnet, 
                                                    newx = test_x, 
                                                    s = 0.01
                                                    )

modelo_error <- bind_rows(modelo_error,
                          list(modelo = "reglineal_glmnet", 
                               error = error(test_prediction$SalePrice, 
                                             test_prediction$predict_reglineal_glmnet)
                               )
                          )


# REG LINEAL LOGARITMOS ---------------------------------------------------

reg_lineal_log <- lm(log(SalePrice)~.-Id, data = houses_training)

summary(reg_lineal2)

test_prediction$predict_reglineal_log <- exp(predict(reg_lineal_log, newdata = houses_test))

modelo_error <- bind_rows(modelo_error,
                          list(modelo = "reg_lineal_log", 
                               error = error(test_prediction$SalePrice, test_prediction$predict_reglineal_log)
                               )
                          )


# GLMNET LOG ---------------------------------------

# Hay variables que pueden no ser relevantes para la predicción.
# La función lm coge todas las variables sin evaluar
# si son importantes en el modelo.

# Hay varios métodos par solucionar el problema.

# El paquete glmnet permite ajustar el modelo a la vez
# que selecciona las variables relevantes.

# install.packages("glmnet")
library(glmnet)
# La variable 262 es la variable objetivo, por eso se exlcuye

train_x <- select(houses_training, -Id, -SalePrice) %>% 
  as.matrix()

reglineal_glmnet_log <- glmnet(x = train_x, y = log(houses_training$SalePrice))


test_x <- select(houses_test, -Id, -SalePrice) %>% 
  as.matrix()
test_x[is.na(test_x)] <- 0

test_prediction$predict_reglineal_glmnet_log <- exp(predict(reglineal_glmnet_log, newx = test_x, s = 0.01))

modelo_error <- bind_rows(modelo_error,
                          list(modelo = "reglineal_glmnet_log", 
                               error = error(test_prediction$SalePrice, test_prediction$predict_reglineal_glmnet_log)
                          )
)


# BOOSTING ----------------------------------------------------------------

# install.packages("xgboost")

library(xgboost)


gradient_boost <- xgboost(data = train_x, 
                          label = houses_training$SalePrice,
                          nrounds = 1000,
                          params = list(eta = 0.01, 
                                        max_depth = 3,
                                        colsample_bytree = 0.75
                                        )
                          )

# El problema con los modelos de ML es que suelen tener asociados
# multitud de metaparámetros que son difíciles de asignar.

test_prediction$gradient_boosting <- predict(gradient_boost, test_x)



modelo_error <- bind_rows(modelo_error,
                          list(modelo = "gradient_boosting", 
                               error = error(test_prediction$SalePrice, test_prediction$gradient_boosting)
                               )
                          )


# RANDOM FOREST -----------------------------------------------------------
# install.packages("randomForest")

library(randomForest)

?randomForest
rf <- randomForest(x = train_x,
                   y = houses_training$SalePrice
                   )

test_prediction$random_forest <- predict(rf, newdata = test_x)


modelo_error <- bind_rows(modelo_error,
                          list(modelo = "random_forest", 
                               error = error(test_prediction$SalePrice, 
                                             test_prediction$random_forest)
                               )
                          )


# ÁRBOL DE REGRESIÓN ------------------------------------------------------

library(rpart)

arbol <- rpart(SalePrice~.-Id, data = houses_training)

test_prediction$predict_arbol <- predict(arbol, newdata = houses_test)

modelo_error <- bind_rows(modelo_error,
                          list(modelo = "arbol", 
                               error = error(test_prediction$SalePrice, 
                                             test_prediction$predict_arbol)
                          )
)

# install.packages("rpart.plot")
rpart.plot::rpart.plot(arbol)


# BOOSTING LOG ----------------------------------------------------------------

# install.packages("xgboost")

library(xgboost)


gradient_boost_log <- xgboost(data = train_x, 
                          label = log(houses_training$SalePrice),
                          nrounds = 1000,
                          params = list(eta = 0.01, 
                                        max_depth = 3,
                                        colsample_bytree = 0.75
                          )
)

# El problema con los modelos de ML es que suelen tener asociados
# multitud de metaparámetros que son difíciles de asignar.

test_prediction$gradient_boost_log <- exp(predict(gradient_boost_log, test_x))



modelo_error <- bind_rows(modelo_error,
                          list(modelo = "gradient_boost_log", 
                               error = error(test_prediction$SalePrice, test_prediction$gradient_boost_log)
                          )
)



# COMPARATIVA FINAL -------------------------------------------------------


modelo_error <- modelo_error %>% arrange(error)
