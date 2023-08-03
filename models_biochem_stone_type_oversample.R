#Stone Type

library(caret)
library(MLeval)
library(keras)
library(tensorflow)
library(pROC)

UK_ST_non_coded_train1_oversample <- UK_ST_non_coded_train1_oversample %>% rename(Stone_type = Class)

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
UK_ST_rf_oversample <-
  train(Stone_type ~ .,
        data = UK_ST_non_coded_train1_oversample,
        trControl = UK_ctrl2,
        method = "rf")

#Partitioning
UK_ST_rpart_oversample <-
  train(Stone_type ~ .,
        data = UK_ST_non_coded_train1_oversample,
        trControl = UK_ctrl2,
        method = "rpart")

#XGBoost
UK_ST_xgboost_oversample <-
  train(Stone_type ~ .,
        data = UK_ST_non_coded_train1_oversample,
        trControl = UK_ctrl2,
        method = "xgbTree")


#Logistic Regression
UK_ST_logit_oversample <-
  glm(Stone_type ~ .,
        data = UK_ST_non_coded_train1_oversample,
        family=binomial)
summary(UK_ST_logit_oversample)


#Neural Net
UK_ST_nn_oversample <-
  train(Stone_type ~ .,
        data = UK_ST_non_coded_train1_oversample,
        trControl = UK_ctrl2,
        method = "nnet")


## Bayesian Generalised Linear Model - not used 

#UK_ST_bayes <-
#  train(Stone_type ~ .,
#        data = UK_ST_non_coded_train1,
#        trControl = UK_ctrl2,
#        method = "bayesglm")


## Recurrence Deep Neural Net
UK_ST_base_keras_oversample <- layer_input(batch_shape = list(NULL, 10))
ST_prediction <- UK_ST_base_keras_oversample %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 5, activation = "softmax")
UK_ST_keras_oversample <- keras_model(UK_ST_base_keras_oversample, ST_prediction)
UK_ST_keras_oversample %>% compile(loss = 'categorical_crossentropy',
                        optimizer = 'rmsprop',
                        metrics = c('accuracy'))
UK_ST_keras_oversample %>% fit(
  UK_ST_train_coded_predictors2_oversample,
  UK_ST_train_coded_outcome3_oversample,
  epochs = 200,
  batch_size = 5,
  validation_split = 0.2,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
  )


