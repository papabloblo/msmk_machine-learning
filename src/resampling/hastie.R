library(ISLR)

?Auto

# Aproximación con validación

set.seed(1)
perc_train <- 0.5

id_train <- sample(1:nrow(Auto), nrow(Auto)*perc_train)

train <- Auto[id_train, ]
test <- Auto[-id_train, ]

lm_fit <- lm(mpg~horsepower, data = train)

rmse <- function(pred, real) mean((pred - real)**2)

# Error en entrenamiento
mod1_rmse_train <- rmse(train$mpg, predict(lm_fit, newdata = train))

# Error en test
mod1_rmse_test <- rmse(test$mpg, predict(lm_fit, newdata = test))

lm_fit2 <- lm(mpg~poly(horsepower, 2), data = train)
lm_fit3 <- lm(mpg~poly(horsepower, 3), data = train)


mod2_rmse_train <- rmse(train$mpg, predict(lm_fit2, newdata = train))
mod2_rmse_test <- rmse(test$mpg, predict(lm_fit2, newdata = test))

mod3_rmse_train <- rmse(train$mpg, predict(lm_fit3, newdata = train))
mod3_rmse_test <- rmse(test$mpg, predict(lm_fit3, newdata = test))

results <- data.frame(mod = 1:3,
                      rmse_train = c(mod1_rmse_train,
                                     mod2_rmse_train,
                                     mod3_rmse_train),
                      rmse_test = c(mod1_rmse_test,
                                     mod2_rmse_test,
                                     mod3_rmse_test)
                      )

plot(Auto$horsepower, Auto$mpg)

set.seed(2)
perc_train <- 0.5

id_train <- sample(1:nrow(Auto), nrow(Auto)*perc_train)

train <- Auto[id_train, ]
test <- Auto[-id_train, ]

lm_fit <- lm(mpg~horsepower, data = train)

rmse <- function(pred, real) mean((pred - real)**2)

# Error en entrenamiento
rmse(train$mpg, predict(lm_fit, newdata = train))

# Error en test
rmse(test$mpg, predict(lm_fit, newdata = test))



# CROSS-VALIDATION --------------------------------------------------------

set.seed(1)
id_sample <- sample(1:nrow(Auto), nrow(Auto))
Auto2 <- Auto[id_sample,]

k_fold <- 10

seq_fold <- seq(1, nrow(Auto2), by = floor(nrow(Auto2)/k_fold))

rmse_train <- numeric(k_fold)
rmse_test <- numeric(k_fold)

for (i in 1:(length(seq_fold) - 1)) {
  id_test <- seq_fold[i]:seq_fold[i+1]
  
  test <- Auto2[id_test,]
  train <- Auto2[-id_test, ]
  
  lm_fit <- lm(mpg~horsepower, data = train)
  
  rmse_train[i] <- rmse(train$mpg, predict(lm_fit, newdata = train))
  rmse_test[i] <- rmse(test$mpg, predict(lm_fit, newdata = test))
}


mean(rmse_train)
sd(rmse_train)

mean(rmse_test)
sd(rmse_test)




