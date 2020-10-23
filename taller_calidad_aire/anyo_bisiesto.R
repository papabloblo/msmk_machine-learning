library(tidyverse)

is.bisiesto <- function(year){
  (year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0))
}

calidad_aire <- data.frame(
  mes = c(1, 2, 2, 2, 2, 4),
  dia = c(31, 29, 29, 29, 29,  31),
  ano = c(2019, 2000, 2019, 2020, 2100, 2019)
)

calidad_aire %>% 
  filter(
    !(mes %in% c(4, 6, 9, 11) & dia == 31),
    !(mes == 2 & is.bisiesto(ano) & dia > 29),
    !(mes == 2 & !is.bisiesto(ano) & dia > 28),
  )
