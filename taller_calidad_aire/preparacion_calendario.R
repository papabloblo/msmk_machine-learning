
calendario <- readr::read_csv2("taller_calidad_aire/data/calendario.csv")

calendario <- calendario[, c(1, 3)]
names(calendario) <- c("fecha", "tipo_dia")

calendario$fecha <- as.Date(calendario$fecha, format = "%d/%m/%Y")

calendario$laborable <- as.integer(calendario$tipo_dia == "laborable")

calendario$tipo_dia <- NULL

saveRDS(calendario, "taller_calidad_aire/data/dias_laborables.RDS")
