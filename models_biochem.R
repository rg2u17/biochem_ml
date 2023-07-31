#Recurrence

library(caret)
library(MLeval)
library(keras)
library(tensorflow)
library(pROC)


UK_ctrl2 <-
  trainControl(
    method = "repeatedcv",
    number = 10,
    classProbs = TRUE,
    savePredictions = TRUE,
    verboseIter = TRUE
  )

#Random Forests
UK_Rec_rf <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1,
        method = "cforest",
        trControl = UK_ctrl2)

#Partitioning
UK_Rec_rpart <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1,
        method = "rpart",
        trControl = UK_ctrl2)

#XGBoost
UK_Rec_xgboost <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1,
        method = "xgbTree",
        trControl = UK_ctrl2)

#Logistic Regression
UK_Rec_logit <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1,
        method = "LogitBoost",
        trControl = UK_ctrl2)

#Neural Net
UK_Rec_nn <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1,
        method = "nnet",
        trControl = UK_ctrl2)

## Bayesian Generalised Linear Model
UK_Rec_bayes <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1,
        method = "bayesglm",
        trControl = UK_ctrl2)

## Recurrence Deep Neural Net
UK_Rec_keras <- keras_model_sequential()
UK_Rec_keras %>% layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
UK_Rec_keras %>% compile(loss = 'binary_crossentropy',
                         optimizer = 'rmsprop',
                         metrics = c('accuracy'))
UK_Rec_keras %>% fit(
  UK_Rec_train_coded_predictors2,
  UK_Rec_train_coded_outcome2,
  epochs = 200,
  batch_size = 5,
  validation_split = 0.2
)


