#' 
#' Minería de datos aplicada a la calidad del aire de la ciudad de madrid
#' 
#' Fuente:
#'   https://datos.madrid.es/sites/v/index.jsp?vgnextoid=8d7357cec5efa610VgnVCM1000001d4a900aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)

# IMPORTACIÓN DE DATOS ----------------------------------------------------

calidad_aire <- readr::read_csv("talleres/calidad_aire/data/calidad_aire_train.csv")


# TRATAMIENTO DE DATOS ----------------------------------------------------

# Nombre de variables en minúscula

names(calidad_aire)
names(calidad_aire) <- tolower(names(calidad_aire))

calidad_aire <- calidad_aire %>% 
  rename(anyo = ano)

# Eliminación de variables unarias
var_unarias <- sapply(calidad_aire, function(x) length(unique(x)))

names(calidad_aire)[var_unarias == 1]

calidad_aire <- calidad_aire %>% 
  select(
    -provincia,
    -municipio
  )

# Formato adecuado

calidad_aire <- calidad_aire %>% 
  mutate(
    anyo = as.integer(anyo),
    mes = as.integer(mes)
  )



# La estructura de los datos no es adecuada para hacer los análisis.
# Por ejemplo, no sería fácil contestar a la pregunta:
#  ¿Cuál es la calidad del aire durante los lunes de enero y febrero?

# La mejor estructura sería tener una variable que indicase la
# fecha y otra con la medida. 
# Además, 


calidad_aire_medicion <- calidad_aire %>% 
  select(
    estacion:mes,
    starts_with("d")
  )

calidad_aire_validado <- calidad_aire %>% 
  select(
    estacion:mes,
    starts_with("v")
  )


calidad_aire_medicion <- calidad_aire_medicion %>% 
  pivot_longer(
    cols = d01:d31,
    names_to = "dia",
    values_to = "medicion"
  )

calidad_aire_medicion <- calidad_aire_medicion %>% 
  mutate(
    dia = as.integer(substr(dia, start = 2, stop = 3)),
    medicion = as.numeric(medicion)
    )



calidad_aire_validado <- calidad_aire_validado %>% 
  pivot_longer(
    cols = v01:v31,
    names_to = "dia",
    values_to = "validado"
  )

calidad_aire_validado <- calidad_aire_validado %>% 
  mutate(
    dia = as.integer(substr(dia, start = 2, stop = 3))
  )


calidad_aire <- calidad_aire_medicion %>% 
  left_join(calidad_aire_validado)

# Eliminar mediciones no validadas (validado = N)
calidad_aire <- calidad_aire %>% 
  filter(validado != "N")


# Varible de fecha

calidad_aire <- calidad_aire %>% 
  mutate(
    fecha = lubridate::make_date(year = anyo, 
                                 month = mes,
                                 day = dia
                                 )
    )


# Agrupación de mediciones por magnitud y fecha

calidad_aire <- calidad_aire %>% 
  group_by(magnitud, 
           anyo, 
           mes, 
           dia, 
           fecha
           ) %>% 
  summarise(medicion_promedio = mean(medicion))



# ANÁLISIS EXPLORATORIO DE DATOS ------------------------------------------

dplyr::glimpse(calidad_aire)

# Eliminación de variables unarias
var_unarias <- sapply(calidad_aire, function(x) length(unique(x)))

names(calidad_aire)[var_unarias == 1]

calidad_aire <- calidad_aire %>% 
  select(
    -PROVINCIA,
    -MUNICIPIO,
    -ANO
  )

# La estructura de los datos no es adecuada para hacer los análisis.
# Por ejemplo, no sería fácil contestar a la pregunta:
#  ¿Cuál es la calidad del aire durante los lunes de enero y febrero?

# La mejor estructura sería tener una variable que indicase la
# fecha y otra con la medida. 
# Además, 


calidad_aire_medicion <- calidad_aire %>% 
  select(
    ESTACION,
    MAGNITUD,
    PUNTO_MUESTREO,
    MES,
    starts_with("D")
  )


calidad_aire_validado <- calidad_aire %>% 
  select(
    ESTACION,
    MAGNITUD,
    PUNTO_MUESTREO,
    MES,
    starts_with("V")
  )

names(calidad_aire)

skimr::skim(calidad_aire)

calidad_aire_medicion <- calidad_aire_medicion %>% 
  pivot_longer(
    cols = D01:D31,
    names_to = "dia",
    values_to = "medicion"
    )

calidad_aire_medicion <- calidad_aire_medicion %>% 
  mutate(
    dia = as.integer(substr(dia, start = 2, stop = 3)),
    MES = as.integer(MES),
    medicion = as.numeric(medicion),
    anyo = 2019, 
    fecha = lubridate::make_date(year = 2019L, 
                                 month = MES,
                                 day = dia
                                 )
  )



calidad <- calidad_aire_medicion %>% 
  group_by(MAGNITUD, fecha) %>% 
  summarise(medicion_promedio = mean(medicion))


calidad %>% 
  ggplot(aes(x = fecha,
             y = medicion_promedio)
         ) + 
  geom_line() +
  facet_wrap(~MAGNITUD, scales = "free")

calidad %>% 
  filter(MAGNITUD == 9) %>% 
  ggplot(aes(x = fecha,
             y = medicion_promedio)
  ) + 
  geom_line()



calidad_aire %>% 
  filter(medicion != V)
  mutate(
    fecha = lubridate::as_date()
  )

