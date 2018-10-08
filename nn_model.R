library(tidyverse)
library(pROC)
test <- sample(nrow(nn_data_90), size = round(nrow(nn_data)/10))
train <- which(!1:nrow(nn_data) %in% test)

getSplit <- function(ds, rows) {
  outcome <- ds$has_AE
  outcome <- to_categorical(outcome, 2)
  
  list(
    outcome = outcome[rows,],
    data = ds[rows,] %>% select(-has_AE) %>% as.matrix()
  )
}

test_data <- getSplit(nn_data_30_wc, test)
train_data <- getSplit(nn_data_30_wc, train)

model <- keras_model_sequential() %>% 
  layer_dense(units = 600, activation = 'relu', input_shape = ncol(test_data$data)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 300, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 200, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 2, activation = 'softmax')

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(lr = 0.001),
  metrics = c('accuracy'))

history <- model %>% fit(
  train_data$data, train_data$outcome, 
  epochs = 20, batch_size = 128, 
  validation_split = 0.2)

model %>% evaluate(test_data$data, test_data$outcome, batch_size = 32)

pred <- model %>% predict(test_data$data, batch_size = 32)
# plot(history)   
#pred
x <- apply(pred, 1, function(v) which.max(v)) %>% factor(labels=c("No", "Yes"))
y <- apply(pred, 1, function(v) which.max(v))

y <- ifelse(y==2, 1, 0)

nnRoc <- data.frame(response = nn_data_90[test, "has_AE"], predictor = y) %>%
  arrange(predictor) %>%
  with(., roc(response, predictor))
auc_nn <- (auc(nnRoc))

res_tbl <- table(true = nn_data_90[test, "has_AE"], test = y)
sens_nn <- res_tbl['1', '1']/(res_tbl['1', '1']+res_tbl['1', '0'])
spec_nn <- res_tbl['0', '0']/(res_tbl['0', '0']+res_tbl['0', '1'])
