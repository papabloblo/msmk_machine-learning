


```{r}
ggplot(data = calidad_aire,
       aes(x = fecha, y = pm25)) + 
  geom_line() +
  facet_wrap(ano~., ncol = 1, scales = "free_x")

ggplot(data = calidad_aire,
       aes(x = lubridate::yday(fecha), y = pm25)) + 
  geom_line(aes(group = ano), alpha = 0.5) +
  geom_smooth()
```


```{r}
ggplot(data = calidad_aire,
       aes(x = lubridate::wday(fecha), y = pm25)) + 
  geom_line(aes(group = paste(lubridate::week(fecha), ano)), alpha = 0.5)
```






## Correlación entre variables

```{r}
cor(calidad_aire %>% select(-ano, -mes, -dia, -fecha))
```

```{r}
calidad_aire <- calidad_aire %>% 
  mutate(pm25_lag = lag(pm25),
         pm10_lag = lag(pm10)
  )

cor(calidad_aire[, c("pm25", "pm25_lag")], use = "complete.obs")
cor(calidad_aire[, c("pm25", "pm10_lag")], use = "complete.obs")

cor(calidad_aire %>% select(-ano, -mes, -dia, -fecha), use = "complete.obs")
```


```{r}
calidad_aire$pm10_lag <- NULL
calidad_aire$pm25_lag <- NULL
```

# Construcción de variables

Recordemos que el objetivo es **predecir el valor de `pm25` en el día posterior**. Eso significa que, si queremos predecir el valor para el 9 de febrero, tenemos que suponer que **solamente conocemos la información hasta el día 8 de febrero**. Por lo tanto, el valor que podremos utilizar de las variables para predecir un día, debe ser la información anterior. Para ello, podemos utilizar la función `lag` en R:
  
  ```{r}
calidad_aire <- calidad_aire %>% 
  arrange(fecha) %>% 
  mutate_at(vars(so2:nmhc), list(lag = lag))
```


```{r}
cor(calidad_aire %>% select(pm25, ends_with("_lag")), use = "complete.obs")
```


```{r}
calidad_aire <- calidad_aire %>% 
  select(ano:dia, fecha, no2_lag:nmhc_lag, pm25)
```

```{r}
calidad_aire
```

¿Tenemos que eliminar alguna variable más? Pensemos en el año. ¿Tendría sentido mantener esta variable? Piensa en el futuro y que querríamos utilizar nuestro modelo para predecir el comportamiento de 2020. Como el valor 2020 no ha existido hasta ahora, el modelo no va a.


También puede parecer relevante el día de la semana. Probablemente, el comportamiento de la calidad del aire será distinta en función del día de la semana, por lo que creamos la variable `dia_semana`

```{r}
calidad_aire$dia_semana <- weekdays(calidad_aire$fecha)
```

```{r}
train <- calidad_aire[calidad_aire$fecha < as.Date("2019-09-01"),]
test <- calidad_aire[calidad_aire$fecha >= as.Date("2019-09-01"),]
```


```{r}
mod_lm <- lm(log(pm25)~.-fecha, data = train)
source("taller_calidad_aire/utils.R")
pred_lm <- exp(predict(mod_lm, newdata = test))

test$pred_lm <- pred_lm
rmse(test$pm25, test$pred_lm)
```

```{r}
p <- ggplot(data = test,
            aes(x = fecha)
) + 
  geom_line(aes(y = pm25)) + 
  geom_line(aes(y = pred_lm), color = "firebrick")

library(plotly)

ggplotly(p)
```



```{r}
library(xgboost)
train_x <- train %>% select(-fecha, -pm25, -pred_lm)
train_x <- as.matrix(train_x)

train_y <- train$pm25

test_x <- test %>% select(-fecha, -pm25, -pred_lm)
test_x <- as.matrix(test_x)

test_y <- test$pm25

dtrain <- xgb.DMatrix(train_x, label = train_y)
dtest <- xgb.DMatrix(test_x, label = test_y)

boost_mod <- xgb.train(data = dtrain, 
                       nrounds = 5000,
                       params = list(
                         eta = 0.01,
                         max_depth = 5,
                         subsample = 0.75,
                         colsample_bytree = 0.3
                       ),
                       early_stopping_rounds = 100,
                       watchlist = list(train = dtrain, eval = dtest)
)

test$pred_boost <- predict(boost_mod, dtest)

rmse(test$pm25, test$pred_boost)

p <- ggplot(data = test,
            aes(x = fecha)
) + 
  geom_line(aes(y = pm25)) + 
  geom_line(aes(y = pred_lm), color = "firebrick") +
  geom_line(aes(y = pred_boost), color = "steelblue")

library(plotly)

ggplotly(p)


import <- xgb.importance(model = boost_mod)

xgb.plot.importance(import)

```

```{r}
library(xgboost)
train_x <- train %>% mutate(dia_sem = as.character(lubridate::wday(fecha, label = TRUE))) %>% select(-fecha, -pm25, -pred_lm)
train_x <- model.matrix(~., train_x)
# train_x <- as.matrix(train_x)

train_y <- train$pm25[2:973]

test_x <- test %>% mutate(dia_sem = as.character(lubridate::wday(fecha, label = TRUE))) %>% select(-fecha, -pm25, -pred_lm, -pred_boost)
test_x <- model.matrix(~., test_x)
# test_x <- as.matrix(test_x)

test_y <- test$pm25

dtrain <- xgb.DMatrix(train_x, label = log(train_y))
dtest <- xgb.DMatrix(test_x, label = log(test_y))

boost_mod <- xgb.train(data = dtrain, 
                       nrounds = 5000,
                       params = list(
                         eta = 0.01,
                         max_depth = 3,
                         subsample = 1,
                         colsample_bytree = 0.3
                       ),
                       early_stopping_rounds = 100,
                       watchlist = list(train = dtrain, eval = dtest)
)

test$pred_boost <- exp(predict(boost_mod, dtest))

rmse(test$pm25, test$pred_boost)

p <- ggplot(data = test,
            aes(x = fecha)
) + 
  geom_line(aes(y = pm25)) + 
  geom_line(aes(y = pred_lm), color = "firebrick") +
  geom_line(aes(y = pred_boost), color = "steelblue")

library(plotly)

ggplotly(p)


import <- xgb.importance(model = boost_mod)

xgb.plot.importance(import)
```


```{r}
test %>% 
  ggplot(aes(x = pm25)) +
  geom_point(aes(y = pred_lm), alpha = 0.6, color = "firebrick") +
  geom_point(aes(y = pred_boost), alpha = 0.6, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0)

test %>% 
  ggplot(aes(x = pm25)) +
  geom_point(aes(y = pred_lm - pm25), alpha = 0.6, color = "firebrick") +
  geom_point(aes(y = pred_boost - pm25), alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0)
```

