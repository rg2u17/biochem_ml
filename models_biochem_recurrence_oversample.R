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
UK_Rec_rf_oversample <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1_oversample,
        method = "cforest",
        trControl = UK_ctrl2)

#Partitioning
UK_Rec_rpart_oversample <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1_oversample,
        method = "rpart",
        trControl = UK_ctrl2)

#XGBoost
UK_Rec_xgboost_oversample <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1_oversample,
        method = "xgbTree",
        trControl = UK_ctrl2)

#Logistic Regression
UK_Rec_logit_oversample <-
  glm(Recurrence ~ .,
        data = UK_Rec_non_coded_train1_oversample,
        family=binomial)
summary(UK_Rec_logit_oversample)


#Neural Net
UK_Rec_nn_oversample <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1_oversample,
        method = "nnet",
        trControl = UK_ctrl2)

## Bayesian Generalised Linear Model
UK_Rec_bayes_oversample <-
  train(Recurrence ~ .,
        data = UK_Rec_non_coded_train1_oversample,
        method = "bayesglm",
        trControl = UK_ctrl2)

## Recurrence Deep Neural Net
UK_Rec_keras_oversample <- keras_model_sequential()
UK_Rec_keras_oversample %>% layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
UK_Rec_keras_oversample %>% compile(loss = 'binary_crossentropy',
                         optimizer = 'rmsprop',
                         metrics = c('accuracy'))
UK_Rec_keras_oversample %>% fit(
  UK_Rec_train_coded_predictors2_oversample,
  UK_Rec_train_coded_outcome2_oversample,
  epochs = 200,
  batch_size = 5,
  validation_split = 0.2,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)


