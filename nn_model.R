library(tidyverse)
library(pROC)

getSplit <- function(ds, rows) {
  outcome <- ds$has_AE
  outcome <- to_categorical(outcome, 2)
  
  list(
    outcome = outcome[rows,],
    data = ds[rows,] %>% select(-has_AE) %>% as.matrix()
  )
}

runModel <- function(ds, epochs = 1e3) {
  test <- sample(nrow(ds), size = round(nrow(ds)/10))
  train <- which(!1:nrow(ds) %in% test)
  test_data <- getSplit(ds, test)
  train_data <- getSplit(ds, train)
  
  input_shape <- ncol(ds) - 1
  network_mdl <- keras_model_sequential() %>% 
    layer_dense(units = floor(input_shape/2), activation = 'relu', input_shape = input_shape) %>% 
    layer_dropout(rate = 0.4) %>% 
    layer_dense(units = floor(input_shape/2), activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = floor(input_shape/2), activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>% 
    layer_dense(units = 2, activation = 'softmax')
  
  network_mdl %>% compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_rmsprop(lr = 0.001),
    metrics = c('accuracy'))
  
  history <- network_mdl %>% fit(
    train_data$data, train_data$outcome, 
    epochs = epochs, batch_size = 128, 
    validation_split = 0.2)
  
  network_mdl %>% evaluate(test_data$data, test_data$outcome, batch_size = 32)
  
  pred <- network_mdl %>% predict(test_data$data, batch_size = 32)
  # plot(history)   
  #pred
  x <- apply(pred, 1, function(v) which.max(v)) %>% factor(labels=c("No", "Yes"))
  y <- apply(pred, 1, function(v) which.max(v))
  
  y <- ifelse(y==2, 1, 0)
  
  nnRoc <- data.frame(response = ds[test, "has_AE"], predictor = y) %>%
    arrange(predictor) %>%
    with(., roc(response, predictor))
  auc_nn <- (auc(nnRoc))
  
  res_tbl <- table(true = ds[test, "has_AE"], test = y)
  sens_nn <- res_tbl['1', '1']/(res_tbl['1', '1']+res_tbl['1', '0'])
  spec_nn <- res_tbl['0', '0']/(res_tbl['0', '0']+res_tbl['0', '1'])
  list(
    auc = auc_nn,
    sens = sens_nn,
    spec = spec_nn
  )
}

runs <- c()
runs <- c(runs, runModel(neural_datasets$`30_wc`))

