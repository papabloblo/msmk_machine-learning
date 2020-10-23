
files <- list.files(path = "taller_calidad_aire/data/raw",
                    pattern = "calidad-aire",
                    full.names = TRUE
                    )


calidad_aire <- purrr::map_df(files, function(x) readr::read_csv2(x))

test <- calidad_aire$ANO == 2019 & calidad_aire$MES %in% c(10, 11, 12)
calidad_aire_train <- calidad_aire[!test, ]
calidad_aire_test <- calidad_aire[test, ]
 
readr::write_csv(calidad_aire_train, path = "taller_calidad_aire/data/calidad_aire_train.csv")
readr::write_csv(calidad_aire_test, path = "taller_calidad_aire/data/calidad_aire_test.csv")
readr::write_csv(calidad_aire, path = "taller_calidad_aire/data/calidad_aire.csv")
