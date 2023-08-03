# Recurrence

UK_Rec_non_coded_test1_recurrence <-
  factor(UK_Rec_non_coded_test1$Recurrence,
         levels = c("No",
                    "Yes"))
Swiss_Rec_non_coded_test1_recurrence <-
  factor(Swiss_Rec_non_coded_test_outcome1,
         levels = c("Yes",
                    "No"))

## Random Forests
UK_Rec_rf_predict_oversample <-
  predict(UK_Rec_rf_oversample, newdata = UK_Rec_non_coded_test_predictors1) %>% factor(levels = c("Yes",
                                                                                                   "No"))
UK_Rec_rf_predict1_oversample <-
  predict(UK_Rec_rf_oversample, newdata = UK_Rec_non_coded_test_predictors1, type = "prob")

UK_Rec_rf_tb_oversample <-
  table(pred = UK_Rec_rf_predict_oversample,
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_rf_tb_oversample)
res_UK_Rec_rf_oversample <- evalm(UK_Rec_rf_oversample)

Swiss_Rec_rf_predict_oversample <-
  predict(UK_Rec_rf_oversample, newdata = Swiss_Rec_non_coded_test_predictors1) %>% factor(levels = c("Yes",
                                                                                                      "No"))
Swiss_Rec_rf_predict1_oversample <-
  predict(UK_Rec_rf_oversample, newdata = Swiss_Rec_non_coded_test_predictors1, type = "prob")
Swiss_Rec_rf_tb_oversample <-
  table(Swiss_Rec_rf_predict_oversample, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_rf_tb_oversample)

UK_Rec_rf_roc_oversample <-
  roc(UK_Rec_non_coded_test1_recurrence,
      UK_Rec_rf_predict1_oversample$Yes)
UK_Rec_rf_auc_oversample <- auc(UK_Rec_rf_roc_oversample)
print(UK_Rec_rf_auc_oversample)
print(ci.auc(UK_Rec_rf_auc_oversample))
varImp(UK_Rec_rf_oversample)

Swiss_Rec_rf_roc_oversample <-
  roc(Swiss_Rec_non_coded_test1_recurrence,
      Swiss_Rec_rf_predict1_oversample$Yes)
Swiss_Rec_rf_auc_oversample <- auc(Swiss_Rec_rf_roc_oversample)
print(Swiss_Rec_rf_auc_oversample)
print(ci.auc(Swiss_Rec_rf_auc_oversample))


## Partitioning
UK_Rec_rpart_predict_oversample <-
  predict(UK_Rec_rpart_oversample, newdata = UK_Rec_non_coded_test_predictors1) %>% factor(levels = c("Yes",
                                                                                                      "No"))
UK_Rec_rpart_predict1_oversample <-
  predict(UK_Rec_rpart_oversample, newdata = UK_Rec_non_coded_test_predictors1, type = "prob")
UK_Rec_rpart_tb_oversample <-
  table(pred = factor(UK_Rec_rpart_predict_oversample),
        ref = UK_Rec_non_coded_test1$Recurrence)
UK_Rec_rpart_tb_oversample
confusionMatrix(UK_Rec_rpart_tb_oversample)
res_UK_Rec_rpart_oversample <- evalm(UK_Rec_rpart_oversample)

Swiss_Rec_rpart_predict_oversample <-
  predict(UK_Rec_rpart_oversample, newdata = Swiss_Rec_non_coded_test_predictors1) %>% factor(levels = c("Yes",
                                                                                                         "No"))
Swiss_Rec_rpart_predict1_oversample <-
  predict(UK_Rec_rpart_oversample, newdata = Swiss_Rec_non_coded_test_predictors1, type = "prob")
Swiss_Rec_rpart_tb_oversample <-
  table(Swiss_Rec_rpart_predict_oversample, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_rpart_tb_oversample)

UK_Rec_rpart_roc_oversample <-
  roc(UK_Rec_non_coded_test1_recurrence,
      UK_Rec_rpart_predict1_oversample$Yes)
UK_Rec_rpart_auc_oversample <- auc(UK_Rec_rpart_roc_oversample)
print(UK_Rec_rpart_auc_oversample)
print(ci.auc(UK_Rec_rpart_auc_oversample))
varImp(UK_Rec_rpart_oversample)

Swiss_Rec_rpart_roc_oversample <-
  roc(Swiss_Rec_non_coded_test1_recurrence,
      Swiss_Rec_rpart_predict1_oversample$Yes)
Swiss_Rec_rpart_auc_oversample <- auc(Swiss_Rec_rpart_roc_oversample)
print(Swiss_Rec_rpart_auc_oversample)
print(ci.auc(Swiss_Rec_rpart_auc_oversample))


## XGBoost
UK_Rec_xgboost_predict_oversample <-
  predict(UK_Rec_xgboost_oversample, newdata = UK_Rec_non_coded_test_predictors1) %>% factor(levels = c("Yes",
                                                                                                        "No"))
UK_Rec_xgboost_predict1_oversample <-
  predict(UK_Rec_xgboost_oversample,
          newdata = UK_Rec_non_coded_test_predictors1,
          type = "prob")
UK_Rec_xgboost_tb_oversample <-
  table(pred = factor(UK_Rec_xgboost_predict_oversample),
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_xgboost_tb_oversample)

Swiss_Rec_xgboost_predict_oversample <-
  predict(UK_Rec_xgboost_oversample, newdata = Swiss_Rec_non_coded_test_predictors1) %>% factor(levels = c("Yes",
                                                                                                           "No"))
Swiss_Rec_xgboost_predict1_oversample <-
  predict(UK_Rec_xgboost_oversample,
          newdata = Swiss_Rec_non_coded_test_predictors1,
          type = "prob")
Swiss_Rec_xgboost_tb_oversample <-
  table(Swiss_Rec_xgboost_predict_oversample, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_xgboost_tb_oversample)

UK_Rec_xgboost_roc_oversample <-
  roc(UK_Rec_non_coded_test1_recurrence,
      UK_Rec_xgboost_predict1_oversample$Yes)
UK_Rec_xgboost_auc_oversample <- auc(UK_Rec_xgboost_roc_oversample)
print(UK_Rec_xgboost_auc_oversample)
print(ci.auc(UK_Rec_xgboost_auc_oversample))
varImp(UK_Rec_xgboost_oversample)

Swiss_Rec_xgboost_roc_oversample <-
  roc(Swiss_Rec_non_coded_test1_recurrence,
      Swiss_Rec_xgboost_predict1_oversample$Yes)
Swiss_Rec_xgboost_auc_oversample <-
  auc(Swiss_Rec_xgboost_roc_oversample)
print(Swiss_Rec_xgboost_auc_oversample)
print(ci.auc(Swiss_Rec_xgboost_auc_oversample))

## Logistic Regression
UK_Rec_logit_predict1_oversample <-
  predict(UK_Rec_logit_oversample, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_logit_predict_oversample <-
  ifelse(UK_Rec_logit_predict1_oversample > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                  "Yes"))

UK_Rec_logit_tb_oversample <-
  table(pred = UK_Rec_logit_predict_oversample,
        ref = UK_Rec_non_coded_test1_recurrence)
confusionMatrix(UK_Rec_logit_tb_oversample)

Swiss_Rec_logit_predict1_oversample <-
  predict(UK_Rec_logit_oversample, newdata = Swiss_Rec_non_coded_test_predictors1)
Swiss_Rec_logit_predict_oversample <-
  ifelse(Swiss_Rec_logit_predict1_oversample > 0, "Yes", "No") %>% factor(levels = c("Yes",
                                                                                     "No"))
Swiss_Rec_logit_tb_oversample <-
  table(Swiss_Rec_logit_predict_oversample, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_logit_tb_oversample)

UK_Rec_logit_roc_oversample <-
  roc(UK_Rec_non_coded_test1_recurrence,
      UK_Rec_logit_predict1_oversample)
UK_Rec_logit_auc_oversample <- auc(UK_Rec_logit_roc_oversample)
print(UK_Rec_logit_auc_oversample)
print(ci.auc(UK_Rec_logit_auc_oversample))
varImp(UK_Rec_logit_oversample)

Swiss_Rec_logit_roc_oversample <-
  roc(Swiss_Rec_non_coded_test1_recurrence,
      Swiss_Rec_logit_predict1_oversample)
Swiss_Rec_logit_auc_oversample <- auc(Swiss_Rec_logit_roc_oversample)
print(Swiss_Rec_logit_auc_oversample)
print(ci.auc(Swiss_Rec_logit_auc_oversample))

## Neural Net
UK_Rec_nn_predict_oversample <-
  predict(UK_Rec_nn_oversample, newdata = UK_Rec_non_coded_test_predictors1) %>% factor(levels = c("Yes",
                                                                                                   "No"))
UK_Rec_nn_predict1_oversample <-
  predict(UK_Rec_nn_oversample, newdata = UK_Rec_non_coded_test_predictors1, type = "prob")
UK_Rec_nn_tb_oversample <-
  table(pred = factor(UK_Rec_nn_predict_oversample),
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_nn_tb_oversample)
res_UK_Rec_nn <- evalm(UK_Rec_nn_oversample)

Swiss_Rec_nn_predict_oversample <-
  predict(UK_Rec_nn_oversample, newdata = Swiss_Rec_non_coded_test_predictors1) %>% factor(levels = c("Yes",
                                                                                                      "No"))
Swiss_Rec_nn_predict1_oversample <-
  predict(UK_Rec_nn_oversample, newdata = Swiss_Rec_non_coded_test_predictors1, type = "prob")
Swiss_Rec_nn_tb_oversample <-
  table(Swiss_Rec_nn_predict_oversample, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_nn_tb_oversample)

UK_Rec_nn_roc_oversample <-
  roc(UK_Rec_non_coded_test1_recurrence,
      UK_Rec_nn_predict1_oversample$Yes)
UK_Rec_nn_auc_oversample <- auc(UK_Rec_nn_roc_oversample)
print(UK_Rec_nn_auc_oversample)
print(ci.auc(UK_Rec_nn_auc_oversample))
varImp(UK_Rec_nn_oversample)

Swiss_Rec_nn_roc_oversample <-
  roc(Swiss_Rec_non_coded_test1_recurrence,
      Swiss_Rec_nn_predict1_oversample$Yes)
Swiss_Rec_nn_auc_oversample <- auc(Swiss_Rec_nn_roc_oversample)
print(Swiss_Rec_nn_auc_oversample)
print(ci.auc(Swiss_Rec_nn_auc_oversample))

## Bayesian Generalised Linear Model
UK_Rec_bayes_predict_oversample <-
  predict(UK_Rec_bayes_oversample, newdata = UK_Rec_non_coded_test_predictors1) %>% factor(levels = c("Yes",
                                                                                                      "No"))
UK_Rec_bayes_predict1_oversample <-
  predict(UK_Rec_bayes_oversample, newdata = UK_Rec_non_coded_test_predictors1, type =
            "prob")
UK_Rec_bayes_tb_oversample <-
  table(pred = factor(UK_Rec_bayes_predict_oversample),
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_bayes_tb_oversample)
res_File_Name_bayes_oversample <- evalm(UK_Rec_bayes_oversample)

Swiss_Rec_bayes_predict_oversample <-
  predict(UK_Rec_bayes_oversample, newdata = Swiss_Rec_non_coded_test_predictors1) %>% factor(levels = c("Yes",
                                                                                                          "No"))
Swiss_Rec_bayes_predict1_oversample <-
  predict(UK_Rec_bayes_oversample, newdata = Swiss_Rec_non_coded_test_predictors1, type = "prob")
Swiss_Rec_bayes_tb_oversample <-
  table(Swiss_Rec_bayes_predict_oversample, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_bayes_tb_oversample)

UK_Rec_bayes_roc_oversample <-
  roc(UK_Rec_non_coded_test1_recurrence,
      UK_Rec_bayes_predict1_oversample$Yes)
UK_Rec_bayes_auc_oversample <- auc(UK_Rec_bayes_roc_oversample)
print(UK_Rec_bayes_auc_oversample)
print(ci.auc(UK_Rec_bayes_auc_oversample))

Swiss_Rec_bayes_roc_oversample <-
  roc(Swiss_Rec_non_coded_test1_recurrence,
      Swiss_Rec_bayes_predict1_oversample$Yes)
Swiss_Rec_bayes_auc_oversample <- auc(Swiss_Rec_bayes_roc_oversample)
print(Swiss_Rec_bayes_auc_oversample)
print(ci.auc(Swiss_Rec_bayes_auc_oversample))

## Recurrence Deep Neural Net
UK_Rec_test_coded_outcome3 <-
  ifelse(UK_Rec_test_coded_outcome2 == 1, "0", "1") %>% factor(levels = c("0", "1"))
Swiss_Rec_test_coded_outcome3 <-
  ifelse(Swiss_Rec_test_coded_outcome2 == 1, "0", "1") %>% factor(levels = c("0", "1"))

UK_Rec_keras_results_oversample <-
  UK_Rec_keras_oversample %>% evaluate(UK_Rec_test_coded_predictors2, UK_Rec_test_coded_outcome2)
UK_Rec_keras_prediction_oversample <-
  UK_Rec_keras_oversample %>% predict(UK_Rec_test_coded_predictors2) %>% as.numeric()
UK_Rec_keras_prediction1_oversample <-
  round(UK_Rec_keras_prediction, digits = 0) %>% factor(levels = c("0", "1"))
UK_Rec_keras_tb_oversample <-
  table(UK_Rec_test_coded_outcome3,
        UK_Rec_keras_prediction1_oversample)
UK_Rec_keras_roc_oversample <-
  roc(UK_Rec_test_coded_outcome3,
      UK_Rec_keras_prediction_oversample)
auc(UK_Rec_keras_roc_oversample)
ci.auc(UK_Rec_keras_roc_oversample)
confusionMatrix(UK_Rec_keras_tb_oversample)
UK_Rec_keras_tb_oversample
plot(UK_Rec_keras_roc_oversample)

Swiss_Rec_keras_prediction_oversample <-
  UK_Rec_keras_oversample %>% predict(Swiss_Rec_test_coded_predictors2) %>% as.numeric()
Swiss_Rec_keras_prediction1_oversample <-
  round(Swiss_Rec_keras_prediction_oversample, digits = 0) %>% factor(levels = c("0", "1"))
Swiss_Rec_keras_tb_oversample <-
  table(Swiss_Rec_test_coded_outcome3,
        Swiss_Rec_keras_prediction1_oversample)
Swiss_Rec_keras_roc_oversample <-
  roc(Swiss_Rec_test_coded_outcome3,
      Swiss_Rec_keras_prediction_oversample)
auc(Swiss_Rec_keras_roc_oversample)
ci.auc(Swiss_Rec_keras_roc_oversample)
confusionMatrix(Swiss_Rec_keras_tb_oversample)
Swiss_Rec_keras_tb_oversample
plot(Swiss_Rec_keras_roc_oversample)

## Build ROC curves
recurrence_roc_list_oversample <- list(
  Swiss_Rec_rf_roc_oversample,
  Swiss_Rec_rpart_roc_oversample,
  Swiss_Rec_xgboost_roc_oversample,
  Swiss_Rec_logit_roc_oversample,
  Swiss_Rec_nn_roc_oversample,
  Swiss_Rec_bayes_roc_oversample,
  Swiss_Rec_keras_roc_oversample
)
