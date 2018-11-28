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

runModel <- function(ds, network_mdl, epochs = 1e1) {
  test <- sample(nrow(ds), size = round(nrow(ds)/10))
  train <- which(!1:nrow(ds) %in% test)
  test_data <- getSplit(ds, test)
  train_data <- getSplit(ds, train)
  
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
  x <- apply(pred, 1, which.max) %>% factor(levels = 1:2, labels=c("No", "Yes"))
  y <- apply(pred, 1, which.max)
  
  y <- ifelse(y==2, 1, 0)
  
  nnRoc <- data.frame(response = ds[test, "has_AE"], predictor = y) %>%
    arrange(predictor) %>%
    with(., roc(response, predictor))
  auc_nn <- (auc(nnRoc))
  
  res_tbl <- table(true = ds[test, "has_AE"], test = y)
  if (nrow(res_tbl) != 2 || ncol(res_tbl) != 2) {
    return(list(auc, sens = NA, spec = NA))
  }
  sens_nn <- res_tbl['1', '1']/(res_tbl['1', '1']+res_tbl['1', '0'])
  spec_nn <- res_tbl['0', '0']/(res_tbl['0', '0']+res_tbl['0', '1'])
  acc_nn <- sum(res_tbl['1', '1']+res_tbl['0', '0'])/sum(res_tbl)
  list(
    sens = sens_nn,
    spec = spec_nn,
    auc = auc_nn,
    acc = acc_nn
  )
}

runModelsOnDs <- function(ds) {
  input_shape <- ncol(ds) - 1
  epochs <- 7e2
  runs <- list()
  runs <- c(runs, 
            list(Shallow = runModel(ds, 
                                    network_mdl = keras_model_sequential() %>% 
                                      layer_dense(units = floor(input_shape/2), 
                                                  activation = 'relu', 
                                                  input_shape = input_shape) %>% 
                                      layer_dropout(rate = 0.4) %>% 
                                      layer_dense(units = floor(input_shape/3), activation = 'relu') %>%
                                      layer_dropout(rate = 0.3) %>% 
                                      layer_dense(units = 2, activation = 'softmax'),
                                    epochs = epochs)))
  
  runs <- c(runs, 
            list(Deep = runModel(ds, 
                                 network_mdl = keras_model_sequential() %>% 
                                   layer_dense(units = floor(input_shape/2), 
                                               activation = 'relu', 
                                               input_shape = input_shape) %>% 
                                   layer_dropout(rate = 0.4) %>% 
                                   layer_dense(units = floor(input_shape/2), activation = 'relu') %>%
                                   layer_dropout(rate = 0.3) %>% 
                                   layer_dense(units = floor(input_shape/3), activation = 'relu') %>%
                                   layer_dropout(rate = 0.3) %>% 
                                   layer_dense(units = floor(input_shape/3), activation = 'relu') %>%
                                   layer_dropout(rate = 0.3) %>% 
                                   layer_dense(units = floor(input_shape/4), activation = 'relu') %>%
                                   layer_dropout(rate = 0.3) %>% 
                                   layer_dense(units = floor(input_shape/4), activation = 'relu') %>%
                                   layer_dropout(rate = 0.3) %>% 
                                   layer_dense(units = 2, activation = 'softmax'),
                                 epochs = epochs)))
  # runs <- c(runs,
  #           list(deep2 = runModel(ds,
  #                                network_mdl = keras_model_sequential() %>%
  #                                  layer_dense(units = floor(input_shape/2),
  #                                              activation = 'relu',
  #                                              input_shape = input_shape) %>%
  #                                  layer_dropout(rate = 0.4) %>%
  #                                  layer_dense(units = floor(input_shape/2), activation = 'relu') %>%
  #                                  layer_dropout(rate = 0.3) %>%
  #                                  layer_dense(units = floor(input_shape/3), activation = 'relu') %>%
  #                                  layer_dropout(rate = 0.3) %>%
  #                                  layer_dense(units = floor(input_shape/3), activation = 'relu') %>%
  #                                  layer_dropout(rate = 0.3) %>%
  #                                  layer_dense(units = floor(input_shape/4), activation = 'relu') %>%
  #                                  layer_dropout(rate = 0.3) %>%
  #                                  layer_dense(units = 2, activation = 'softmax'),
  #                                epochs = epochs)))
  runs <- c(runs, 
            list(Wide = runModel(ds, 
                                 network_mdl = keras_model_sequential() %>% 
                                   layer_dense(units = floor(input_shape/2), 
                                               activation = 'relu', 
                                               input_shape = input_shape) %>% 
                                   layer_dropout(rate = 0.4) %>% 
                                   layer_dense(units = input_shape, activation = 'relu') %>%
                                   layer_dropout(rate = 0.3) %>% 
                                   layer_dense(units = floor(input_shape/2), activation = 'relu') %>%
                                   layer_dropout(rate = 0.3) %>% 
                                   layer_dense(units = 2, activation = 'softmax'),
                                 epochs = epochs)))
  runs
}

results_nn <- sapply(neural_datasets, simplify = FALSE, FUN = runModelsOnDs)

tmp_nn <- lapply(results_nn, function(x) do.call(rbind, x))
htmlTable(
  do.call(rbind, tmp) %>% txtRound(digits=2),
  rgroup = names(tmp),
  n.rgroup = sapply(tmp, nrow)
)
z <- data.frame(tmp_nn)  
z <- z[,c(3, 7, 11, 15)]


z[, 'Model'] <- row.names(z)

z <- data.frame(matrix(unlist(z), nrow=3, ncol = 5, byrow=F))
names(z) <- c("30 days with codes", "30 days","90 days with codes", "90 days", "Model")
z <- z[,c(2,1,4,3,5)]


z$`30 days` <- as.numeric(as.character(z$`30 days`))
z$`30 days with codes` <- as.numeric(as.character(z$`30 days with codes`))
z$`90 days` <- as.numeric(as.character(z$`90 days`))
z$`90 days with codes` <- as.numeric(as.character(z$`90 days with codes`))


y <- melt(z, id = "Model")

ggplot(y, aes(Model, value)) + 
  geom_bar(aes(fill = Model), position = "dodge", stat="identity", color = "steel blue") +
  facet_grid(.~variable) +
  coord_cartesian(ylim=c(0.55, .76)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), title = element_text(size = 18) ) +
  scale_fill_brewer(palette=1, direction = -1) +
  labs(title = "AUC for the different neural networks",x = "Shape of neural network", y = "AUC") +
  guides(fill=FALSE) 