

  outcome <- nn_data_30_wc$has_AE
  outcome <- to_categorical(outcome, 2)
  
for_nn_30 <- list(
    outcome = outcome,
    data = nn_data_30_wc %>% select(-has_AE) %>% as.matrix()
  )

calcSensNn <- function(x) {
  y <- x['1', '1']/sum(x[ ,'1'])
}
calcSpecNn <- function(x) {
  y <- x['0', '0']/sum(x[ ,'0'])
}

getSensSpecNn <- function(my_data, getModelResults, no_splits = 10) {
  shuffled <- sample(nrow(my_data$outcome), replace = FALSE)
  batch_size <- floor(length(shuffled)/no_splits)
  results <- list()
  for (i in 0:(no_splits-1)) {
    if (i == no_splits - 1) {
      test_rows <- shuffled[(i*batch_size + 1):length(shuffled)]
    } else {
      test_rows <- shuffled[1:batch_size + i*batch_size]
    }
    train_rows <- shuffled[!(shuffled %in% test_rows)]
    a <- my_data$outcome[train_rows,]
    b <- my_data$data[train_rows,]
    train <- list(outcome = a, data = b)
    c <- my_data$outcome[-train_rows,]
    d <- my_data$data[-train_rows,]
    test <- list(outcome = c, data = d)
    results <- append(results, getModelResults(train = train, test = test))
  }
  c(sens = calcSensNn(results[[1]]) %>%
      mean(),
    spec = calcSpecNn(results[[1]]) %>%
      mean(),
    AUC = results[[2]] %>%
     mean())
}

code_results_30[['NN']] <- getSensSpecNn(for_nn_30, function(train, test) {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 49, activation = 'relu', input_shape = ncol(test$data)) %>% 
    layer_dropout(rate = 0.4) %>% 
    layer_dense(units = 400, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 400, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>% 
    layer_dense(units = 300, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 300, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 200, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 200, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 100, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 2, activation = 'softmax')
  
  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_rmsprop(lr = 0.001),
    metrics = c('accuracy'))
  
  history <- model %>% fit(
    train_data$data, train_data$outcome, 
    epochs = 100, batch_size = 128, 
    validation_split = 0.2)
model %>% evaluate(test$data, test$outcome, batch_size = 32)
pred <- model %>% predict(test$data, batch_size = 32)
y <- apply(pred, 1, function(v) which.max(v))
y <- ifelse(y==2, 1, 0)
nnRoc <- data.frame(response = test$outcome[,2], predictor = y) %>%
  arrange(predictor) %>%
  with(., roc(response, predictor))
auc_nn <- (auc(nnRoc))

list(table(predicted = y, true=test$outcome[,2]), auc_nn)
})
nn_res

#  sens      spec       AUC 
# 0.9054054 0.4561404 0.6807729 