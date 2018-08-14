### Data Preprocessing ###
library(data.table)
library(foreach)
# retrieve filenames of test sets
test_filenames = list.files(pattern = "blogData_test")
# load and combine dataset
train = fread("blogData_train.csv")
test = foreach(i = 1:length(test_filenames), .combine = rbind) %do% 
{temp = fread(test_filenames[i], header = F)
}
# log-transform
train[, V281 := log(1 + V281)]
test[, V281 := log(1 + V281)]

# drop continous variables without variation
drop = c(8, 13, 28, 33, 38, 40, 43, 50, 278)
train[, (drop) := NULL]
test[, (drop) := NULL]

# write to files
write.csv(train, "BlogFeedback-Train.csv", row.names = F)
write.csv(test, "BlogFeedback-Test.csv", row.names = F)
### Basic Models ###
library(data.table)
library(MatrixModels)

library(e1071)
library(FNN)
library(glmnet)
library(ranger)
library(xgboost)

# load and combine dataset
train = fread("BlogFeedback-Train.csv")
test = fread("BlogFeedback-Test.csv")

# error measure
mse = function(y_hat, y) {
  mse = mean((y - y_hat)^2)
  
  return(mse)
}
# try kNN
pred_knn = knn.reg(train_x, test_x, train_y, k = 19)$pred
mse(pred_knn, test_y)

# try LASSO
mdl_lasso = cv.glmnet(train_x_sparse, train_y, family = "gaussian", alpha = 1)
pred_lasso = predict(mdl_lasso, newx = test_x)
mse(pred_lasso, test_y)
# try random forest
mdl_rf = ranger(V281 ~ ., data = train, num.trees = 1000, mtry = 120, write.forest = T)
pred_rf = predict(mdl_rf, test)
mse(pred_rf$predictions, test_y)
