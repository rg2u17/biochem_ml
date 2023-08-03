# Recurrence

UK_Rec_non_coded_test1_recurrence <- factor(UK_Rec_non_coded_test1$Recurrence,
                                            levels = c("Yes",
                                            "No"))
Swiss_Rec_non_coded_test1_recurrence <- factor(Swiss_Rec_non_coded_test_outcome1,
                                               levels = c("Yes",
                                                          "No"))

## Random Forests
UK_Rec_rf_predict_imputed <-
  predict(UK_Rec_rf_imputed, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_rf_predict1_imputed<-
  predict(UK_Rec_rf_imputed, newdata = UK_Rec_non_coded_test_predictors1, type = "prob")

UK_Rec_rf_tb_imputed <-
  table(UK_Rec_rf_predict_imputed,
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_rf_tb_imputed)
res_UK_Rec_rf_imputed <- evalm(UK_Rec_rf_imputed)

Swiss_Rec_rf_predict_imputed <-
  predict(UK_Rec_rf_imputed, newdata = Swiss_Rec_non_coded_test_predictors1)
Swiss_Rec_rf_predict1_imputed <-
  predict(UK_Rec_rf_imputed, newdata = Swiss_Rec_non_coded_test_predictors1, type = "prob")
Swiss_Rec_rf_tb_imputed <-
  table(Swiss_Rec_rf_predict_imputed, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_rf_tb_imputed)

UK_Rec_rf_roc_imputed <-
  roc(
    UK_Rec_non_coded_test1_recurrence,
    UK_Rec_rf_predict1_imputed$Yes
    )
UK_Rec_rf_auc_imputed<-auc(UK_Rec_rf_roc_imputed)
print(UK_Rec_rf_auc_imputed)
print(ci.auc(UK_Rec_rf_auc_imputed))
varImp(UK_Rec_rf_imputed)

Swiss_Rec_rf_roc_imputed <-
  roc(
    Swiss_Rec_non_coded_test1_recurrence,
    Swiss_Rec_rf_predict1_imputed$Yes
  )
Swiss_Rec_rf_auc_imputed<-auc(Swiss_Rec_rf_roc_imputed)
print(Swiss_Rec_rf_auc_imputed)
print(ci.auc(Swiss_Rec_rf_auc_imputed))


## Partitioning
UK_Rec_rpart_predict_imputed <-
  predict(UK_Rec_rpart_imputed, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_rpart_predict1_imputed <-
  predict(UK_Rec_rpart_imputed, newdata = UK_Rec_non_coded_test_predictors1, type = "prob")
UK_Rec_rpart_tb_imputed <-
  table(pred = UK_Rec_rpart_predict_imputed,
        ref = UK_Rec_non_coded_test1$Recurrence)
UK_Rec_rpart_tb_imputed
confusionMatrix(UK_Rec_rpart_tb_imputed)
res_UK_Rec_rpart_imputed <- evalm(UK_Rec_rpart_imputed)

Swiss_Rec_rpart_predict_imputed <-
  predict(UK_Rec_rpart_imputed, newdata = Swiss_Rec_non_coded_test_predictors1)
Swiss_Rec_rpart_predict1_imputed <-
  predict(UK_Rec_rpart_imputed, newdata = Swiss_Rec_non_coded_test_predictors1, type = "prob")
Swiss_Rec_rpart_tb_imputed <-
  table(Swiss_Rec_rpart_predict_imputed, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_rpart_tb_imputed)

UK_Rec_rpart_roc_imputed <-
  roc(
    UK_Rec_non_coded_test1_recurrence,
    UK_Rec_rpart_predict1_imputed$Yes
  )
UK_Rec_rpart_auc_imputed<-auc(UK_Rec_rpart_roc_imputed)
print(UK_Rec_rpart_auc_imputed)
print(ci.auc(UK_Rec_rpart_auc_imputed))
varImp(UK_Rec_rpart_imputed)

Swiss_Rec_rpart_roc_imputed <-
  roc(
    Swiss_Rec_non_coded_test1_recurrence,
    Swiss_Rec_rpart_predict1_imputed$Yes
  )
Swiss_Rec_rpart_auc_imputed<-auc(Swiss_Rec_rpart_roc_imputed)
print(Swiss_Rec_rpart_auc_imputed)
print(ci.auc(Swiss_Rec_rpart_auc_imputed))


## XGBoost
UK_Rec_xgboost_predict_imputed <-
  predict(UK_Rec_xgboost_imputed, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_xgboost_predict1_imputed <-
  predict(UK_Rec_xgboost_imputed, newdata = UK_Rec_non_coded_test_predictors1, type = "prob")
UK_Rec_xgboost_tb_imputed <-
  table(pred = UK_Rec_xgboost_predict_imputed,
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_xgboost_tb_imputed)

Swiss_Rec_xgboost_predict_imputed <-
  predict(UK_Rec_xgboost_imputed, newdata = Swiss_Rec_non_coded_test_predictors1)
Swiss_Rec_xgboost_predict1_imputed <-
  predict(UK_Rec_xgboost_imputed, newdata = Swiss_Rec_non_coded_test_predictors1, type = "prob")
Swiss_Rec_xgboost_tb_imputed <-
  table(Swiss_Rec_xgboost_predict_imputed, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_xgboost_tb_imputed)

UK_Rec_xgboost_roc_imputed <-
  roc(
    UK_Rec_non_coded_test1_recurrence,
    UK_Rec_xgboost_predict1_imputed$Yes
  )
UK_Rec_xgboost_auc_imputed<-auc(UK_Rec_xgboost_roc_imputed)
print(UK_Rec_xgboost_auc_imputed)
print(ci.auc(UK_Rec_xgboost_auc_imputed))
varImp(UK_Rec_xgboost_imputed)

Swiss_Rec_xgboost_roc_imputed <-
  roc(
    Swiss_Rec_non_coded_test1_recurrence,
    Swiss_Rec_xgboost_predict1_imputed$Yes
  )
Swiss_Rec_xgboost_auc_imputed<-auc(Swiss_Rec_xgboost_roc_imputed)
print(Swiss_Rec_xgboost_auc_imputed)
print(ci.auc(Swiss_Rec_xgboost_auc_imputed))

## Logistic Regression
UK_Rec_logit_predict1_imputed <-
  predict(UK_Rec_logit_imputed, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_logit_predict_imputed <-
  ifelse(UK_Rec_logit_predict1_imputed > 0, "Yes", "No") %>% factor(levels = c("Yes",
                                                                       "No"))

UK_Rec_logit_tb_imputed <-
  table(pred = UK_Rec_logit_predict_imputed,
        ref = UK_Rec_non_coded_test1_recurrence)
confusionMatrix(UK_Rec_logit_tb_imputed)

Swiss_Rec_logit_predict1_imputed <-
  predict(UK_Rec_logit_imputed, newdata = Swiss_Rec_non_coded_test_predictors1)
Swiss_Rec_logit_predict_imputed <-
  ifelse(Swiss_Rec_logit_predict1_imputed > 0, "Yes", "No") %>% factor(levels = c("Yes",
                                                                            "No"))
Swiss_Rec_logit_tb_imputed <-
  table(Swiss_Rec_logit_predict_imputed, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_logit_tb_imputed)

UK_Rec_logit_roc_imputed <-
  roc(
    UK_Rec_non_coded_test1_recurrence,
    UK_Rec_logit_predict1_imputed
  )
UK_Rec_logit_auc_imputed<-auc(UK_Rec_logit_roc_imputed)
print(UK_Rec_logit_auc_imputed)
print(ci.auc(UK_Rec_logit_auc_imputed))
varImp(UK_Rec_logit_imputed)

Swiss_Rec_logit_roc_imputed <-
  roc(
    Swiss_Rec_non_coded_test1_recurrence,
    Swiss_Rec_logit_predict1_imputed
  )
Swiss_Rec_logit_auc_imputed<-auc(Swiss_Rec_logit_roc_imputed)
print(Swiss_Rec_logit_auc_imputed)
print(ci.auc(Swiss_Rec_logit_auc_imputed))

## Neural Net
UK_Rec_nn_predict_imputed <-
  predict(UK_Rec_nn_imputed, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_nn_predict1_imputed <-
  predict(UK_Rec_nn_imputed, newdata = UK_Rec_non_coded_test_predictors1, type = "prob")
UK_Rec_nn_tb_imputed <-
  table(pred = UK_Rec_nn_predict_imputed,
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_nn_tb_imputed)
res_UK_Rec_nn <- evalm(UK_Rec_nn_imputed)

Swiss_Rec_nn_predict_imputed <-
  predict(UK_Rec_nn_imputed, newdata = Swiss_Rec_non_coded_test_predictors1)
Swiss_Rec_nn_predict1_imputed <-
  predict(UK_Rec_nn_imputed, newdata = Swiss_Rec_non_coded_test_predictors1, type = "prob")
Swiss_Rec_nn_tb_imputed <-
  table(Swiss_Rec_nn_predict_imputed, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_nn_tb_imputed)

UK_Rec_nn_roc_imputed <-
  roc(
    UK_Rec_non_coded_test1_recurrence,
    UK_Rec_nn_predict1_imputed$Yes
  )
UK_Rec_nn_auc_imputed<-auc(UK_Rec_nn_roc_imputed)
print(UK_Rec_nn_auc_imputed)
print(ci.auc(UK_Rec_nn_auc_imputed))
varImp(UK_Rec_nn_imputed)

Swiss_Rec_nn_roc_imputed <-
  roc(
    Swiss_Rec_non_coded_test1_recurrence,
    Swiss_Rec_nn_predict1_imputed$Yes
  )
Swiss_Rec_nn_auc_imputed<-auc(Swiss_Rec_nn_roc_imputed)
print(Swiss_Rec_nn_auc_imputed)
print(ci.auc(Swiss_Rec_nn_auc_imputed))

## Bayesian Generalised Linear Model
UK_Rec_bayes_predict_imputed <-
  predict(UK_Rec_bayes_imputed, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_bayes_predict1_imputed <-
  predict(UK_Rec_bayes_imputed, newdata = UK_Rec_non_coded_test_predictors1, type ="prob")
UK_Rec_bayes_tb_imputed <-
  table(pred = UK_Rec_bayes_predict_imputed,
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_bayes_tb_imputed)
res_File_Name_bayes_imputed <- evalm(UK_Rec_bayes_imputed)

Swiss_Rec_bayes_predict_imputed <-
  predict(UK_Rec_bayes_imputed, newdata = Swiss_Rec_non_coded_test_predictors1)
Swiss_Rec_bayes_predict1_imputed <-
  predict(UK_Rec_bayes_imputed, newdata = Swiss_Rec_non_coded_test_predictors1, type = "prob")
Swiss_Rec_bayes_tb_imputed <-
  table(Swiss_Rec_bayes_predict_imputed, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_bayes_tb_imputed)

UK_Rec_bayes_roc_imputed <-
  roc(
    UK_Rec_non_coded_test1_recurrence,
    UK_Rec_bayes_predict1_imputed$Yes
  )
UK_Rec_bayes_auc_imputed<-auc(UK_Rec_bayes_roc_imputed)
print(UK_Rec_bayes_auc_imputed)
print(ci.auc(UK_Rec_bayes_auc_imputed))

Swiss_Rec_bayes_roc_imputed <-
  roc(
    Swiss_Rec_non_coded_test1_recurrence,
    Swiss_Rec_bayes_predict1_imputed$Yes
  )
Swiss_Rec_bayes_auc_imputed<-auc(Swiss_Rec_bayes_roc_imputed)
print(Swiss_Rec_bayes_auc_imputed)
print(ci.auc(Swiss_Rec_bayes_auc_imputed))

## Recurrence Deep Neural Net
UK_Rec_test_coded_outcome3 <-
  ifelse(UK_Rec_test_coded_outcome2 == 1, "0", "1") %>% factor(levels = c("0", "1"))
Swiss_Rec_test_coded_outcome3 <-
  ifelse(Swiss_Rec_test_coded_outcome2 == 1, "0", "1") %>% factor(levels = c("0", "1"))

UK_Rec_keras_results_imputed <-
  UK_Rec_keras_imputed %>% evaluate(subset(UK_Rec_test_coded_predictors2, select = -Stone.Coded), UK_Rec_test_coded_outcome2)
UK_Rec_keras_prediction_imputed <-
  UK_Rec_keras_imputed %>% predict(subset(UK_Rec_test_coded_predictors2, select = -Stone.Coded)) %>% as.numeric()
UK_Rec_keras_prediction1_imputed <-
  round(UK_Rec_keras_prediction, digits = 0) %>% factor(levels = c("0", "1"))
UK_Rec_keras_tb_imputed <-
  table(UK_Rec_test_coded_outcome3, UK_Rec_keras_prediction1_imputed)
UK_Rec_keras_roc_imputed <-
  roc(UK_Rec_test_coded_outcome3, UK_Rec_keras_prediction_imputed)
auc(UK_Rec_keras_roc_imputed)
ci.auc(UK_Rec_keras_roc_imputed)
confusionMatrix(UK_Rec_keras_tb_imputed) 
UK_Rec_keras_tb_imputed
plot(UK_Rec_keras_roc_imputed)

Swiss_Rec_keras_prediction_imputed <-
  UK_Rec_keras_imputed %>% predict(subset(Swiss_Rec_test_coded_predictors2, select = -Stone.Coded)) %>% as.numeric()
Swiss_Rec_keras_prediction1_imputed <-
  round(Swiss_Rec_keras_prediction_imputed, digits = 0) %>% factor(levels = c("0", "1"))
Swiss_Rec_keras_tb_imputed <-
  table(Swiss_Rec_test_coded_outcome3, Swiss_Rec_keras_prediction1_imputed)
Swiss_Rec_keras_roc_imputed <-
  roc(Swiss_Rec_test_coded_outcome3, Swiss_Rec_keras_prediction_imputed)
auc(Swiss_Rec_keras_roc_imputed)
ci.auc(Swiss_Rec_keras_roc_imputed)
confusionMatrix(Swiss_Rec_keras_tb_imputed) 
Swiss_Rec_keras_tb_imputed
plot(Swiss_Rec_keras_roc_imputed)

## Build ROC curves
recurrence_roc_list_imputed <- list(Swiss_Rec_rf_roc_imputed,
                            Swiss_Rec_rpart_roc_imputed,
                            Swiss_Rec_xgboost_roc_imputed,
                            Swiss_Rec_logit_roc_imputed,
                            Swiss_Rec_nn_roc_imputed,
                            Swiss_Rec_bayes_roc_imputed,
                            Swiss_Rec_keras_roc_imputed)
