test_kg <- read_csv("data/raw/test.csv")
sample_sub <- read_csv("data/raw/sample_submission.csv")

test_kg$GrLivArea <- test_kg$GrLivArea/10.764

test_kg <- bake(train_recipe, new_data = test_kg)


pred_test_kg <- select(test_kg, -Id) %>% 
  as.matrix()

pred_test_kg[is.na(pred_test_kg)] <- 0

test_kg$SalePrice <- exp(predict(reglineal_glmnet_log,
                            newx = pred_test_kg, 
                            s = 0.01))[, "1"]


subida_kg <- test_kg %>% 
  select(Id, SalePrice)

write_csv(subida_kg, "data/subida_glmnet_log.csv")
