# create design matrices
train_x = model.Matrix(V281 ~ . - 1, data = train, sparse = F)
train_y = train$V281

test_x = model.Matrix(V281 ~ . - 1, data = test, sparse = F)
test_y = test$V281

train_xgb = xgb.DMatrix(data = as.matrix(train_x), label = train_y)
test_xgb = xgb.DMatrix(data = as.matrix(test_x), label = test_y)

# fit XGBoost
pred_xgb = foreach(i = 1:n, .combine = cbind) %do% {
  mdl_xgb = xgboost(data = train_xgb, nround = 750, nthread = 4, max_depth = 6, eta = 0.025, subsample = 0.7, gamma = 3)
  
  return(predict(mdl_xgb, test_xgb))
}
# fit random forest
pred_rf = foreach(i = 1:n, .combine = cbind) %do% {
  mdl_rf = ranger(V281 ~ ., data = train, num.trees = 1000, mtry = 120, write.forest = T)
  
  return(predict(mdl_rf, test)$predictions)
}
