calcSens <- function(x) {
  y <- x['Yes', 'Yes']/sum(x[ ,'Yes'])
}
calcSpec <- function(x) {
  y <- x['No', 'No']/sum(x[ ,'No'])
}

getSensSpec <- function(my_data, getModelResults, no_splits = 10) {
  shuffled <- sample(nrow(my_data), replace = FALSE)
  batch_size <- floor(length(shuffled)/no_splits)
  results <- list()
  for (i in 0:(no_splits-1)) {
    if (i == no_splits - 1) {
      test_rows <- shuffled[(i*batch_size + 1):length(shuffled)]
    } else {
      test_rows <- shuffled[1:batch_size + i*batch_size]
    }
    train_rows <- shuffled[!(shuffled %in% test_rows)]
    train <- my_data[train_rows, ]
    test <- my_data[test_rows, ]
    
    results <- append(results, getModelResults(train = train, test = test))
  }
  
  c(sens = sapply(results, calcSens) %>%
      mean(),
    spec = sapply(results, calcSpec) %>%
      mean())
}
all_results[['Random forest weighted']] <- getSensSpec(no_na_data, function(train, test) {
  cross_model <- ranger(has_AE~., data = train)
  p <- predict(cross_model, data = test)
  list(table(predicted = p$predictions, true=test$has_AE))
})