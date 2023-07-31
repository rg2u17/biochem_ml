# Recurrence

## Random Forests
UK_Rec_rf_predict <-
  predict(UK_Rec_rf, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_rf_tb <-
  table(UK_Rec_rf_predict, ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_rf_tb)
res_UK_Rec_rf <- evalm(UK_Rec_rf)
Swiss_Rec_rf_predict <-
  predict(UK_Rec_rf, newdata = Swiss_Rec_non_coded_test_predictors1)
Swiss_Rec_rf_tb <-
  table(Swiss_Rec_rf_predict, ref = Swiss_Rec_non_coded_test1$Recurrence)
confusionMatrix(Swiss_Rec_rf_tb)

## Partitioning
UK_Rec_rpart_predict <-
  predict(UK_Rec_rpart, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_rpart_tb <-
  table(pred = factor(UK_Rec_rpart_predict),
        ref = UK_Rec_non_coded_test1$Recurrence)
#Does not predict any with recurrence therefore cannot produce confusionMatrix
UK_Rec_rpart_tb
#confusionMatrix(UK_Rec_rpart_tb)
res_UK_Rec_rpart <- evalm(UK_Rec_rpart)

## XGBoost
UK_Rec_xgboost_predict <-
  predict(UK_Rec_xgboost, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_xgboost_tb <-
  table(pred = factor(UK_Rec_xgboost_predict),
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_xgboost_tb)
res_UK_Rec_xgboost <- evalm(UK_Rec_xgboost)

## Logistic Regression
UK_Rec_logit_predict <-
  predict(UK_Rec_logit, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_logit_tb <-
  table(pred = factor(UK_Rec_logit_predict),
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_logit_tb)
res_UK_Rec_logit <- evalm(UK_Rec_logit)

## Neural Net
UK_Rec_nn_predict <-
  predict(UK_Rec_nn, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_nn_tb <-
  table(pred = factor(UK_Rec_nn_predict),
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_nn_tb)
res_File_Name_nn <- evalm(UK_Rec_nn)

## Bayesian Generalised Linear Model
UK_Rec_bayes_predict <-
  predict(UK_Rec_bayes, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_bayes_tb <-
  table(pred = factor(UK_Rec_bayes_predict),
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_bayes_tb)
res_File_Name_bayes <- evalm(UK_Rec_bayes)

## Recurrence Deep Neural Net
UK_Rec_keras_results <-
  UK_Rec_keras %>% evaluate(UK_Rec_test_coded_predictors2, UK_Rec_test_coded_outcome2)
UK_Rec_keras_prediction <-
  UK_Rec_keras %>% predict(UK_Rec_test_coded_predictors2)
UK_Rec_keras_prediction1 <- round(UK_Rec_keras_prediction, digits = 0)
UK_Rec_keras_tb <-
  table(UK_Rec_test_coded_outcome2, UK_Rec_keras_prediction1)
UK_Rec_keras_roc <-
  roc(UK_Rec_test_coded_outcome2, UK_Rec_keras_prediction1)
auc(UK_Rec_keras_roc)
#confusionMatrix(UK_Rec_keras_tb) - does not predict any as having recurrence
UK_Rec_keras_tb
plot(UK_Rec_keras_roc)
