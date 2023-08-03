# Stone Type

## Random Forests
UK_ST_rf_predict_imputed <-
  predict(UK_ST_rf_imputed, newdata = UK_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                           "CaP",
                                                                           "Unknown",
                                                                           "Urate"))
UK_ST_rf_predict1_imputed <-
  predict(UK_ST_rf_imputed, newdata = UK_ST_non_coded_test1, type = "prob")
UK_ST_rf_tb_imputed <-
  table(UK_ST_rf_predict_imputed, ref = UK_ST_non_coded_test1$Stone_type)
confusionMatrix(UK_ST_rf_tb_imputed)
res_UK_ST_rf_imputed <- evalm(UK_ST_rf_imputed)

Swiss_ST_rf_predict_imputed <-
  predict(UK_ST_rf_imputed, newdata = Swiss_ST_all_non_coded) %>% factor(levels = c("CaOx",
                                                                            "CaP",
                                                                            "Unknown",
                                                                            "Urate"))
Swiss_ST_rf_predict1_imputed <-
  predict(UK_ST_rf_imputed, newdata = Swiss_ST_all_non_coded, type = "prob")

Swiss_ST_rf_tb_imputed <-
  table(Swiss_ST_rf_predict_imputed, ref = Swiss_ST_all_non_coded$Stone_type)
confusionMatrix(Swiss_ST_rf_tb_imputed)

Swiss_ST_rf_multiroc_imputed <-
  multiclass.roc(Swiss_ST_all_non_coded$Stone_type,
                 Swiss_ST_rf_predict1_imputed)
Swiss_ST_rf_caox_roc_imputed <- roc(Swiss_ST_all_non_coded_caox,
                            Swiss_ST_rf_predict1_imputed$CaOx)
Swiss_ST_rf_cap_roc_imputed <- roc(Swiss_ST_all_non_coded_cap,
                           Swiss_ST_rf_predict1_imputed$CaP)
Swiss_ST_rf_urate_roc_imputed <- roc(Swiss_ST_all_non_coded_urate,
                             Swiss_ST_rf_predict1_imputed$Urate)
auc(Swiss_ST_rf_multiroc_imputed)
auc(Swiss_ST_rf_caox_roc_imputed)
ci.auc(Swiss_ST_rf_caox_roc_imputed)
auc(Swiss_ST_rf_cap_roc_imputed)
ci.auc(Swiss_ST_rf_cap_roc_imputed)
auc(Swiss_ST_rf_urate_roc_imputed)
ci.auc(Swiss_ST_rf_urate_roc_imputed)


## Partitioning
UK_ST_rpart_predict_imputed <-
  predict(UK_ST_rpart_imputed, newdata = UK_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                              "CaP",
                                                                              "Unknown",
                                                                              "Urate"))
UK_ST_rpart_tb_imputed <-
  table(pred = UK_ST_rpart_predict_imputed,
        ref = UK_ST_non_coded_test1$Stone_type)
confusionMatrix(UK_ST_rpart_tb_imputed)
UK_ST_rpart_tb_imputed
res_UK_ST_rpart <- evalm(UK_ST_rpart_imputed)

Swiss_ST_rpart_predict_imputed <-
  predict(UK_ST_rpart_imputed, newdata = Swiss_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                                 "CaP",
                                                                                 "Unknown",
                                                                                 "Urate"))
Swiss_ST_rpart_predict1_imputed <-
  predict(UK_ST_rpart_imputed, newdata = Swiss_ST_non_coded_test1, type = "prob")

Swiss_ST_rpart_tb_imputed <-
  table(Swiss_ST_rpart_predict_imputed, ref = Swiss_ST_non_coded_test1$Stone_type)
confusionMatrix(Swiss_ST_rpart_tb_imputed)

Swiss_ST_rpart_multiroc_imputed <-
  multiclass.roc(Swiss_ST_non_coded_test1$Stone_type,
                 Swiss_ST_rpart_predict1_imputed)
Swiss_ST_rpart_caox_roc_imputed <- roc(Swiss_non_coded_test_caox,
                               Swiss_ST_rpart_predict1_imputed$CaOx)
Swiss_ST_rpart_cap_roc_imputed <- roc(Swiss_non_coded_test_cap,
                              Swiss_ST_rpart_predict1_imputed$CaP)
Swiss_ST_rpart_urate_roc_imputed <- roc(Swiss_non_coded_test_urate,
                                Swiss_ST_rpart_predict1_imputed$Urate)
auc(Swiss_ST_rpart_multiroc_imputed)
auc(Swiss_ST_rpart_caox_roc_imputed)
ci.auc(Swiss_ST_rpart_caox_roc_imputed)
auc(Swiss_ST_rpart_cap_roc_imputed)
ci.auc(Swiss_ST_rpart_cap_roc_imputed)
auc(Swiss_ST_rpart_urate_roc_imputed)
ci.auc(Swiss_ST_rpart_urate_roc_imputed)

## XGBoost
UK_ST_xgboost_predict_imputed <-
  predict(UK_ST_xgboost_imputed, UK_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                      "CaP",
                                                                      "Unknown",
                                                                      "Urate"))
UK_ST_xgboost_tb_imputed <-
  table(pred = UK_ST_xgboost_predict_imputed, ref = UK_ST_non_coded_test1$Stone_type)
confusionMatrix(UK_ST_xgboost_tb_imputed)
UK_ST_xgboost_tb_imputed
res_UK_ST_xgboost <- evalm(UK_ST_xgboost_imputed)

Swiss_ST_xgboost_predict_imputed <-
  predict(UK_ST_xgboost_imputed, newdata = Swiss_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                                   "CaP",
                                                                                   "Unknown",
                                                                                   "Urate"))
Swiss_ST_xgboost_predict1_imputed <-
  predict(UK_ST_xgboost_imputed, newdata = Swiss_ST_non_coded_test1, type = "prob")

Swiss_ST_xgboost_tb_imputed <-
  table(Swiss_ST_xgboost_predict_imputed, ref = Swiss_ST_non_coded_test1$Stone_type)
confusionMatrix(Swiss_ST_xgboost_tb_imputed)

Swiss_ST_xgboost_multiroc_imputed <-
  multiclass.roc(Swiss_ST_non_coded_test1$Stone_type,
                 Swiss_ST_xgboost_predict1_imputed)
Swiss_ST_xgboost_caox_roc_imputed <- roc(Swiss_non_coded_test_caox,
                                 Swiss_ST_xgboost_predict1_imputed$CaOx)
Swiss_ST_xgboost_cap_roc_imputed <- roc(Swiss_non_coded_test_cap,
                                Swiss_ST_xgboost_predict1_imputed$CaP)
Swiss_ST_xgboost_urate_roc_imputed <- roc(Swiss_non_coded_test_urate,
                                  Swiss_ST_xgboost_predict1_imputed$Urate)
auc(Swiss_ST_xgboost_multiroc_imputed)
auc(Swiss_ST_xgboost_caox_roc_imputed)
ci.auc(Swiss_ST_xgboost_caox_roc_imputed)
auc(Swiss_ST_xgboost_cap_roc_imputed)
ci.auc(Swiss_ST_xgboost_cap_roc_imputed)
auc(Swiss_ST_xgboost_urate_roc_imputed)
ci.auc(Swiss_ST_xgboost_urate_roc_imputed)


## Neural Net
UK_ST_nn_predict_imputed <-
  predict(UK_ST_nn_imputed, UK_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                 "CaP",
                                                                 "Unknown",
                                                                 "Urate"))
UK_ST_nn_tb_imputed <-
  table(pred = UK_ST_nn_predict_imputed,
        ref = UK_ST_non_coded_test1$Stone_type)
confusionMatrix(UK_ST_nn_tb_imputed)
UK_ST_nn_tb_imputed
res_UK_ST_nn <- evalm(UK_ST_nn_imputed)

Swiss_ST_nn_predict_imputed <-
  predict(UK_ST_nn_imputed, newdata = Swiss_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                              "CaP",
                                                                              "Unknown",
                                                                              "Urate"))
Swiss_ST_nn_predict1_imputed <-
  predict(UK_ST_nn_imputed, newdata = Swiss_ST_non_coded_test1, type = "prob")

Swiss_ST_nn_tb_imputed <-
  table(Swiss_ST_nn_predict_imputed, ref = Swiss_ST_non_coded_test1$Stone_type)
confusionMatrix(Swiss_ST_nn_tb_imputed)

Swiss_ST_nn_multiroc_imputed <-
  multiclass.roc(Swiss_ST_non_coded_test1$Stone_type,
                 Swiss_ST_nn_predict1_imputed)
Swiss_ST_nn_caox_roc_imputed <- roc(Swiss_non_coded_test_caox,
                            Swiss_ST_nn_predict1_imputed$CaOx)
Swiss_ST_nn_cap_roc_imputed <- roc(Swiss_non_coded_test_cap,
                           Swiss_ST_nn_predict1_imputed$CaP)
Swiss_ST_nn_urate_roc_imputed <- roc(Swiss_non_coded_test_urate,
                             Swiss_ST_nn_predict1_imputed$Urate)
auc(Swiss_ST_nn_multiroc_imputed)
auc(Swiss_ST_nn_caox_roc_imputed)
ci.auc(Swiss_ST_nn_caox_roc_imputed)
auc(Swiss_ST_nn_cap_roc_imputed)
ci.auc(Swiss_ST_nn_cap_roc_imputed)
auc(Swiss_ST_nn_urate_roc_imputed)
ci.auc(Swiss_ST_nn_urate_roc_imputed)

## Recurrence Deep Neural Net
UK_ST_keras_results_imputed <-
  UK_ST_keras_imputed %>% evaluate(UK_ST_test_coded_predictors2,
                           UK_ST_test_coded_outcome3,
                           verbose = 1)
UK_ST_keras_prediction_imputed <-
  UK_ST_keras_imputed %>% predict(UK_ST_test_coded_predictors2,
                          batch_size = 32,
                          verbose = 1)
UK_ST_keras_prediction1_imputed <- round(UK_ST_keras_prediction_imputed, digits = 0)
UK_ST_keras_tb_imputed <-
  table(UK_ST_test_coded_outcome3, UK_ST_keras_prediction1_imputed)
UK_ST_keras_roc_imputed <-
  roc(UK_ST_test_coded_outcome3, UK_ST_keras_prediction1_imputed)
auc(UK_ST_keras_roc_imputed)
confusionMatrix(UK_ST_keras_tb_imputed)
UK_ST_keras_tb_imputed

Swiss_ST_keras_prediction_imputed <-
  UK_ST_keras_imputed %>% predict(Swiss_ST_test_coded_predictors2,
                          batch_size = 32,
                          verbose = 1)
Swiss_ST_keras_prediction1_imputed <-
  round(Swiss_ST_keras_prediction_imputed, digits = 0)
Swiss_ST_keras_prediction2_imputed <- as_tibble(Swiss_ST_keras_prediction_imputed)
colnames(Swiss_ST_keras_prediction2_imputed) <- c("Unclear",
                                          "CaOx",
                                          "CaP",
                                          "Unknown",
                                          "Urate")
Swiss_ST_keras_prediction2_imputed <- Swiss_ST_keras_prediction2_imputed[, -1]
Swiss_ST_keras_prediction3_imputed <- Swiss_ST_keras_prediction2_imputed
Swiss_ST_keras_prediction3_imputed$CaOx <-
  ifelse(Swiss_ST_keras_prediction3_imputed$CaOx > 0.4, "1", "0") %>% factor(levels = c("0",
                                                                                "1"))
Swiss_ST_keras_prediction3_imputed$CaP <-
  ifelse(Swiss_ST_keras_prediction3_imputed$CaP > 0.4, "1", "0") %>% factor(levels = c("0",
                                                                               "1"))
Swiss_ST_keras_prediction3_imputed$Unknown <-
  ifelse(Swiss_ST_keras_prediction3_imputed$Unknown > 0.4, "1", "0") %>% factor(levels = c("0",
                                                                                   "1"))
Swiss_ST_keras_prediction3_imputed$Urate <-
  ifelse(Swiss_ST_keras_prediction3_imputed$Urate > 0.4, "1", "0") %>% factor(levels = c("0",
                                                                                 "1"))

Swiss_ST_keras_tb_imputed <-
  table(
    as.matrix(Swiss_ST_test_coded_outcome3),
    as.matrix(Swiss_ST_keras_prediction3_imputed)
  )
confusionMatrix(Swiss_ST_keras_tb_imputed)

Swiss_ST_keras_caox_roc_imputed <- roc(Swiss_ST_test_coded_outcome3$CaOx,
                               Swiss_ST_keras_prediction2_imputed$CaOx)
Swiss_ST_keras_cap_roc_imputed <- roc(Swiss_ST_test_coded_outcome3$CaP,
                              Swiss_ST_keras_prediction2_imputed$CaP)
Swiss_ST_keras_urate_roc_imputed <- roc(Swiss_ST_test_coded_outcome3$Urate,
                                Swiss_ST_keras_prediction2_imputed$Urate)
auc(Swiss_ST_keras_multiroc_imputed)
auc(Swiss_ST_keras_caox_roc_imputed)
ci.auc(Swiss_ST_keras_caox_roc_imputed)
auc(Swiss_ST_keras_cap_roc_imputed)
ci.auc(Swiss_ST_keras_cap_roc_imputed)
auc(Swiss_ST_keras_urate_roc_imputed)
ci.auc(Swiss_ST_keras_urate_roc_imputed)


## Sort ROC lists
caox_roc_list_imputed <- list(
  Swiss_ST_rf_caox_roc_imputed,
  Swiss_ST_rpart_caox_roc_imputed,
  Swiss_ST_xgboost_caox_roc_imputed,
  Swiss_ST_nn_caox_roc_imputed,
  Swiss_ST_keras_caox_roc_imputed
)

cap_roc_list_imputed <- list(
  Swiss_ST_rf_cap_roc_imputed,
  Swiss_ST_rpart_cap_roc_imputed,
  Swiss_ST_xgboost_cap_roc_imputed,
  Swiss_ST_nn_cap_roc_imputed,
  Swiss_ST_keras_cap_roc_imputed
)

urate_roc_list_imputed <- list(
  Swiss_ST_rf_urate_roc_imputed,
  Swiss_ST_rpart_urate_roc_imputed,
  Swiss_ST_xgboost_urate_roc_imputed,
  Swiss_ST_nn_urate_roc_imputed,
  Swiss_ST_keras_urate_roc_imputed
)
