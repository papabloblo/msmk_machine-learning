

calidad_aire %>%
  filter(magnitud == 9) %>% 
  ggplot(aes(x = fecha,
             y = medicion_promedio)
  ) + 
  geom_line() + 
  ylim(0, NA) +
  facet_wrap(~anyo, ncol = 1, scales = "free_x")


calidad_aire %>% 
  mutate(dia_semana = lubridate::wday(fecha, week_start = 1),
         semana = lubridate::week(fecha)) %>% 
  filter(magnitud == 9) %>% 
  ggplot(
    aes(x = dia_semana,
        y = medicion_promedio,
        group = paste(semana, anyo))
  ) + 
  geom_line(alpha = 0.2)


# EJERCICIO:
# Pensar qué variables pueden afectar a la calidad del aire
# Pensar qué variables NO se deberían usar
# Buscar cuáles se pueden calcular
# Calcular:
# Medición del día anterior
# Medición media de la semana
# Medición media del mismo día de la semana
# Festivo
# Meteorología del día anterior

# Episodios de contaminación: https://datos.madrid.es/sites/v/index.jsp?vgnextoid=1ae9d1caa41be610VgnVCM1000001d4a900aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD