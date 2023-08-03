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
    verboseIter = TRUE,
    p = 1
  )

#Random Forests
UK_Rec_rf_imputed <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1_imputed,
        method = "cforest",
        trControl = UK_ctrl2)

#Partitioning
UK_Rec_rpart_imputed <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1_imputed,
        method = "rpart",
        trControl = UK_ctrl2)

#XGBoost
UK_Rec_xgboost_imputed <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1_imputed,
        method = "xgbTree",
        trControl = UK_ctrl2)

#Logistic Regression
UK_Rec_logit_imputed <-
  glm(Recurrence ~ .,
        data = UK_Rec_non_coded_train1_imputed,
        family=binomial)
summary(UK_Rec_logit_imputed)


#Neural Net
UK_Rec_nn_imputed <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1_imputed,
        method = "nnet",
        trControl = UK_ctrl2)

## Bayesian Generalised Linear Model
UK_Rec_bayes_imputed <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1_imputed,
        method = "bayesglm",
        trControl = UK_ctrl2)

## Recurrence Deep Neural Net
UK_Rec_keras_imputed <- keras_model_sequential()
UK_Rec_keras_imputed %>% layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
UK_Rec_keras_imputed %>% compile(loss = 'binary_crossentropy',
                         optimizer = 'rmsprop',
                         metrics = c('accuracy'))
UK_Rec_keras_imputed %>% fit(
  UK_Rec_train_coded_predictors2_imputed,
  UK_Rec_train_coded_outcome2_imputed,
  epochs = 200,
  batch_size = 5,
  validation_split = 0.2,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)


