# Recurrence

UK_Rec_non_coded_test1_recurrence <- factor(UK_Rec_non_coded_test1$Recurrence,
                                            levels = c("Yes",
                                            "No"))
Swiss_Rec_non_coded_test1_recurrence <- factor(Swiss_Rec_non_coded_test1$Recurrence,
                                               levels = c("Yes",
                                                          "No"))

## Random Forests
UK_Rec_rf_predict <-
  predict(UK_Rec_rf, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_rf_predict1<-
  predict(UK_Rec_rf, newdata = UK_Rec_non_coded_test_predictors1, type = "prob")

UK_Rec_rf_tb <-
  table(UK_Rec_rf_predict,
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_rf_tb)
res_UK_Rec_rf <- evalm(UK_Rec_rf)

Swiss_Rec_rf_predict <-
  predict(UK_Rec_rf, newdata = Swiss_Rec_non_coded_test_predictors1)
Swiss_Rec_rf_predict1 <-
  predict(UK_Rec_rf, newdata = Swiss_Rec_non_coded_test_predictors1, type = "prob")
Swiss_Rec_rf_tb <-
  table(Swiss_Rec_rf_predict, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_rf_tb)

UK_Rec_rf_roc <-
  roc(
    UK_Rec_non_coded_test1_recurrence,
    UK_Rec_rf_predict1$Yes
    )
UK_Rec_rf_auc<-auc(UK_Rec_rf_roc)
print(UK_Rec_rf_auc)
print(ci.auc(UK_Rec_rf_auc))
varImp(UK_Rec_rf)

Swiss_Rec_rf_roc <-
  roc(
    Swiss_Rec_non_coded_test1_recurrence,
    Swiss_Rec_rf_predict1$Yes
  )
Swiss_Rec_rf_auc<-auc(Swiss_Rec_rf_roc)
print(Swiss_Rec_rf_auc)
print(ci.auc(Swiss_Rec_rf_auc))


## Partitioning
UK_Rec_rpart_predict <-
  predict(UK_Rec_rpart, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_rpart_predict1 <-
  predict(UK_Rec_rpart, newdata = UK_Rec_non_coded_test_predictors1, type = "prob")
UK_Rec_rpart_tb <-
  table(pred = factor(UK_Rec_rpart_predict),
        ref = UK_Rec_non_coded_test1$Recurrence)
UK_Rec_rpart_tb
confusionMatrix(UK_Rec_rpart_tb)
res_UK_Rec_rpart <- evalm(UK_Rec_rpart)

Swiss_Rec_rpart_predict <-
  predict(UK_Rec_rpart, newdata = Swiss_Rec_non_coded_test_predictors1)
Swiss_Rec_rpart_predict1 <-
  predict(UK_Rec_rpart, newdata = Swiss_Rec_non_coded_test_predictors1, type = "prob")
Swiss_Rec_rpart_tb <-
  table(Swiss_Rec_rpart_predict, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_rpart_tb)

UK_Rec_rpart_roc <-
  roc(
    UK_Rec_non_coded_test1_recurrence,
    UK_Rec_rpart_predict1$Yes
  )
UK_Rec_rpart_auc<-auc(UK_Rec_rpart_roc)
print(UK_Rec_rpart_auc)
print(ci.auc(UK_Rec_rpart_auc))
varImp(UK_Rec_rpart)

Swiss_Rec_rpart_roc <-
  roc(
    Swiss_Rec_non_coded_test1_recurrence,
    Swiss_Rec_rpart_predict1$Yes
  )
Swiss_Rec_rpart_auc<-auc(Swiss_Rec_rpart_roc)
print(Swiss_Rec_rpart_auc)
print(ci.auc(Swiss_Rec_rpart_auc))


## XGBoost
UK_Rec_xgboost_predict <-
  predict(UK_Rec_xgboost, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_xgboost_predict1 <-
  predict(UK_Rec_xgboost, newdata = UK_Rec_non_coded_test_predictors1, type = "prob")
UK_Rec_xgboost_tb <-
  table(pred = factor(UK_Rec_xgboost_predict),
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_xgboost_tb)

Swiss_Rec_xgboost_predict <-
  predict(UK_Rec_xgboost, newdata = Swiss_Rec_non_coded_test_predictors1)
Swiss_Rec_xgboost_predict1 <-
  predict(UK_Rec_xgboost, newdata = Swiss_Rec_non_coded_test_predictors1, type = "prob")
Swiss_Rec_xgboost_tb <-
  table(Swiss_Rec_xgboost_predict, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_xgboost_tb)

UK_Rec_xgboost_roc <-
  roc(
    UK_Rec_non_coded_test1_recurrence,
    UK_Rec_xgboost_predict1$Yes
  )
UK_Rec_xgboost_auc<-auc(UK_Rec_xgboost_roc)
print(UK_Rec_xgboost_auc)
print(ci.auc(UK_Rec_xgboost_auc))
varImp(UK_Rec_xgboost)

Swiss_Rec_xgboost_roc <-
  roc(
    Swiss_Rec_non_coded_test1_recurrence,
    Swiss_Rec_xgboost_predict1$Yes
  )
Swiss_Rec_xgboost_auc<-auc(Swiss_Rec_xgboost_roc)
print(Swiss_Rec_xgboost_auc)
print(ci.auc(Swiss_Rec_xgboost_auc))

## Logistic Regression
UK_Rec_logit_predict1 <-
  predict(UK_Rec_logit, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_logit_predict <-
  ifelse(UK_Rec_logit_predict1 > 0, "Yes", "No") %>% factor(levels = c("Yes",
                                                                       "No"))

UK_Rec_logit_tb <-
  table(pred = UK_Rec_logit_predict,
        ref = UK_Rec_non_coded_test1_recurrence)
confusionMatrix(UK_Rec_logit_tb)

Swiss_Rec_logit_predict1 <-
  predict(UK_Rec_logit, newdata = Swiss_Rec_non_coded_test_predictors1)
Swiss_Rec_logit_predict <-
  ifelse(Swiss_Rec_logit_predict1 > 0, "Yes", "No") %>% factor(levels = c("Yes",
                                                                            "No"))
Swiss_Rec_logit_tb <-
  table(Swiss_Rec_logit_predict, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_logit_tb)

UK_Rec_logit_roc <-
  roc(
    UK_Rec_non_coded_test1_recurrence,
    UK_Rec_logit_predict1
  )
UK_Rec_logit_auc<-auc(UK_Rec_logit_roc)
print(UK_Rec_logit_auc)
print(ci.auc(UK_Rec_logit_auc))
varImp(UK_Rec_logit)

Swiss_Rec_logit_roc <-
  roc(
    Swiss_Rec_non_coded_test1_recurrence,
    Swiss_Rec_logit_predict1
  )
Swiss_Rec_logit_auc<-auc(Swiss_Rec_logit_roc)
print(Swiss_Rec_logit_auc)
print(ci.auc(Swiss_Rec_logit_auc))

## Neural Net
UK_Rec_nn_predict <-
  predict(UK_Rec_nn, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_nn_predict1 <-
  predict(UK_Rec_nn, newdata = UK_Rec_non_coded_test_predictors1, type = "prob")
UK_Rec_nn_tb <-
  table(pred = factor(UK_Rec_nn_predict),
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_nn_tb)
res_UK_Rec_nn <- evalm(UK_Rec_nn)

Swiss_Rec_nn_predict <-
  predict(UK_Rec_nn, newdata = Swiss_Rec_non_coded_test_predictors1)
Swiss_Rec_nn_predict1 <-
  predict(UK_Rec_nn, newdata = Swiss_Rec_non_coded_test_predictors1, type = "prob")
Swiss_Rec_nn_tb <-
  table(Swiss_Rec_nn_predict, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_nn_tb)

UK_Rec_nn_roc <-
  roc(
    UK_Rec_non_coded_test1_recurrence,
    UK_Rec_nn_predict1$Yes
  )
UK_Rec_nn_auc<-auc(UK_Rec_nn_roc)
print(UK_Rec_nn_auc)
print(ci.auc(UK_Rec_nn_auc))
varImp(UK_Rec_nn)

Swiss_Rec_nn_roc <-
  roc(
    Swiss_Rec_non_coded_test1_recurrence,
    Swiss_Rec_nn_predict1$Yes
  )
Swiss_Rec_nn_auc<-auc(Swiss_Rec_nn_roc)
print(Swiss_Rec_nn_auc)
print(ci.auc(Swiss_Rec_nn_auc))

## Bayesian Generalised Linear Model
UK_Rec_bayes_predict <-
  predict(UK_Rec_bayes, newdata = UK_Rec_non_coded_test_predictors1)
UK_Rec_bayes_predict1 <-
  predict(UK_Rec_bayes, newdata = UK_Rec_non_coded_test_predictors1, type ="prob")
UK_Rec_bayes_tb <-
  table(pred = factor(UK_Rec_bayes_predict),
        ref = UK_Rec_non_coded_test1$Recurrence)
confusionMatrix(UK_Rec_bayes_tb)
res_File_Name_bayes <- evalm(UK_Rec_bayes)

Swiss_Rec_bayes_predict <-
  predict(UK_Rec_bayes, newdata = Swiss_Rec_non_coded_test_predictors1)
Swiss_Rec_bayes_predict1 <-
  predict(UK_Rec_bayes, newdata = Swiss_Rec_non_coded_test_predictors1, type = "prob")
Swiss_Rec_bayes_tb <-
  table(Swiss_Rec_bayes_predict, ref = Swiss_Rec_non_coded_test1_recurrence)
confusionMatrix(Swiss_Rec_bayes_tb)

UK_Rec_bayes_roc <-
  roc(
    UK_Rec_non_coded_test1_recurrence,
    UK_Rec_bayes_predict1$Yes
  )
UK_Rec_bayes_auc<-auc(UK_Rec_bayes_roc)
print(UK_Rec_bayes_auc)
print(ci.auc(UK_Rec_bayes_auc))
varImp(UK_Rec_bayes)

Swiss_Rec_bayes_roc <-
  roc(
    Swiss_Rec_non_coded_test1_recurrence,
    Swiss_Rec_bayes_predict1$Yes
  )
Swiss_Rec_bayes_auc<-auc(Swiss_Rec_bayes_roc)
print(Swiss_Rec_bayes_auc)
print(ci.auc(Swiss_Rec_bayes_auc))

## Recurrence Deep Neural Net
UK_Rec_test_coded_outcome3 <-
  ifelse(UK_Rec_test_coded_outcome2 == 1, "0", "1") %>% factor(levels = c("0", "1"))
Swiss_Rec_test_coded_outcome3 <-
  ifelse(Swiss_Rec_test_coded_outcome2 == 1, "0", "1") %>% factor(levels = c("0", "1"))

UK_Rec_keras_results <-
  UK_Rec_keras %>% evaluate(UK_Rec_test_coded_predictors2, UK_Rec_test_coded_outcome2)
UK_Rec_keras_prediction <-
  UK_Rec_keras %>% predict(UK_Rec_test_coded_predictors2) %>% as.numeric()
UK_Rec_keras_prediction1 <-
  round(UK_Rec_keras_prediction, digits = 0) %>% factor(levels = c("0", "1"))
UK_Rec_keras_tb <-
  table(UK_Rec_test_coded_outcome3, UK_Rec_keras_prediction1)
UK_Rec_keras_roc <-
  roc(UK_Rec_test_coded_outcome3, UK_Rec_keras_prediction)
auc(UK_Rec_keras_roc)
ci.auc(UK_Rec_keras_roc)
confusionMatrix(UK_Rec_keras_tb) 
UK_Rec_keras_tb
plot(UK_Rec_keras_roc)

Swiss_Rec_keras_prediction <-
  UK_Rec_keras %>% predict(Swiss_Rec_test_coded_predictors2) %>% as.numeric()
Swiss_Rec_keras_prediction1 <-
  round(Swiss_Rec_keras_prediction, digits = 0) %>% factor(levels = c("0", "1"))
Swiss_Rec_keras_tb <-
  table(Swiss_Rec_test_coded_outcome3, Swiss_Rec_keras_prediction1)
Swiss_Rec_keras_roc <-
  roc(Swiss_Rec_test_coded_outcome3, Swiss_Rec_keras_prediction)
auc(Swiss_Rec_keras_roc)
ci.auc(Swiss_Rec_keras_roc)
confusionMatrix(Swiss_Rec_keras_tb) 
Swiss_Rec_keras_tb
plot(Swiss_Rec_keras_roc)

## Build ROC curves
recurrence_roc_list <- list(Swiss_Rec_rf_roc,
                            Swiss_Rec_rpart_roc,
                            Swiss_Rec_xgboost_roc,
                            Swiss_Rec_logit_roc,
                            Swiss_Rec_nn_roc,
                            Swiss_Rec_bayes_roc,
                            Swiss_Rec_keras_roc)
