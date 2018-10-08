test_vs_train = list()
class(test_vs_train) <- c("results_class", class(test_vs_train))
print.results_class <- function(l) {
  if (length(l) == 0) {
    cat('\n None')
    return()
  }
  
  sapply(l, USE.NAMES = TRUE, simplify = FALSE, function(vals) {
    sapply(vals, function(v) {
      sprintf("%.3f", v)
    })
  }) %>% 
    do.call(rbind, .) %>% 
    knitr::kable() %>% 
    print
}

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
    #print(results)
  }
  c(sens_train = calcSens(results[[1]]) %>%
      mean(),
    spec_train = calcSpec(results[[1]]) %>%
      mean(),
    sens_test = calcSens(results[[2]]) %>%
       mean(),
     spec_test = calcSpec(results[[2]]) %>%
       mean())
}
set.seed(333)
x <- getSensSpec(data_90, function(train, test) {
  cross_model_rf <- randomForest(has_AE~., data = train, ntree = 800, mtry = 3, replace = F, nodesize = 3)
  p_tr <- predict(cross_model_rf)
  p_te <- predict(cross_model_rf, newdata = test)
  list(table(predicted = p_tr, true = train$has_AE), 
       table(predicted = p_te, true = test$has_AE))
})

y <- getSensSpec(data_90, function(train, test) {
    cross_model_log <- glm(has_AE ~., 
                           family = binomial(link = logit), 
                           data = train)
    p_tr <- predict(cross_model_log, newdata = train, type = "response")
    p_te <- predict(cross_model_log, newdata = test, type = "response")
    p_tr <- ifelse(p_tr > 0.5, "Yes", "No")
    p_te <- ifelse(p_te > 0.5, "Yes", "No")
    list(table(predicted = p_tr, true=train$has_AE), 
         table(predicted = p_te, true=test$has_AE))
  })


z<- getSensSpec(data_90, function(train, test) {
  cross_model_svm <- svm(has_AE~., data = train)
  p_tr <- predict(cross_model_svm, newdata = train)
  p_te <- predict(cross_model_svm, newdata = test)
  list(table(predicted = p_tr, true=train$has_AE), 
       table(predicted = p_te, true=test$has_AE))
}) 

q <- getSensSpec(data_90, function(train, test) {
  cross_model_log <- glm(has_AE ~los, 
                         family = binomial(link = logit), 
                         data = train)
  p_tr <- predict(cross_model_log, newdata = train, type = "response")
  p_te <- predict(cross_model_log, newdata = test, type = "response")
  p_tr <- ifelse(p_tr > 0.5, "Yes", "No")
  p_te <- ifelse(p_te > 0.5, "Yes", "No")
  list(table(predicted = p_tr, true=train$has_AE), 
       table(predicted = p_te, true=test$has_AE))
})





x <- round(x, digits = 3)
y <- round(y, digits = 3)
z <- round(z, digits = 3)
q <- round(q, digits = 3)
my_output <- list(
  `Random forest`= data.frame(sens_train = x[1], spec_train = x[2], sens_test=x[3], spec_test = x[4]),
   `Logistic regression`= data.frame(sens_train = y[1], spec_train = y[2], sens_test=y[3], spec_test = y[4]),
  `Support vector machine` = data.frame(sens_train = z[1], spec_train = z[2], sens_test=z[3], spec_test = z[4]),
  `log 3 variables` = data.frame(sens_train = q[1], spec_train = q[2], sens_test=q[3], spec_test = q[4])) 
  

htmlTable(
  do.call(rbind, my_output),
  rgroups = names(my_output),
  n.rgroup = sapply(my_output, nrow),
  digits = 3)

test_rows <- sample(nrow(y), 132)
t <- y[-test_rows,]
z <- y[test_rows,]

mylog <- glm(has_AE~., data = train_set, family = binomial(link = logit))
p <- predict(mylog, newdata = test_set, type = "response")
p <- ifelse(p > 0.5, "Yes", "No")
x <- table(predicted = p, true = test_set$has_AE)
sens <- calcSens(x)
spec <- calcSpec(x)
