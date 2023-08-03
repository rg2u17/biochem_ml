# Stone Type
## Random Forests
UK_ST_rf_predict_oversample <-
  predict(UK_ST_rf_oversample, newdata = UK_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                           "CaP",
                                                                           "Unknown",
                                                                           "Urate"))
UK_ST_rf_predict1_oversample <-
  predict(UK_ST_rf_oversample, newdata = UK_ST_non_coded_test1, type = "prob")
UK_ST_rf_tb_oversample <-
  table(UK_ST_rf_predict_oversample, ref = UK_ST_non_coded_test1$Stone_type)
confusionMatrix(UK_ST_rf_tb_oversample)
res_UK_ST_rf_oversample <- evalm(UK_ST_rf_oversample)

Swiss_ST_rf_predict_oversample <-
  predict(UK_ST_rf_oversample, newdata = Swiss_ST_all_non_coded) %>% factor(levels = c("CaOx",
                                                                            "CaP",
                                                                            "Unknown",
                                                                            "Urate"))
Swiss_ST_rf_predict1_oversample <-
  predict(UK_ST_rf_oversample, newdata = Swiss_ST_all_non_coded, type = "prob")

Swiss_ST_rf_tb_oversample <-
  table(Swiss_ST_rf_predict_oversample, ref = Swiss_ST_all_non_coded$Stone_type)
confusionMatrix(Swiss_ST_rf_tb_oversample)

Swiss_ST_rf_multiroc_oversample <-
  multiclass.roc(Swiss_ST_all_non_coded$Stone_type,
                 Swiss_ST_rf_predict1_oversample)
Swiss_ST_rf_caox_roc_oversample <- roc(Swiss_ST_all_non_coded_caox,
                            Swiss_ST_rf_predict1_oversample$CaOx)
Swiss_ST_rf_cap_roc_oversample <- roc(Swiss_ST_all_non_coded_cap,
                           Swiss_ST_rf_predict1_oversample$CaP)
Swiss_ST_rf_urate_roc_oversample <- roc(Swiss_ST_all_non_coded_urate,
                             Swiss_ST_rf_predict1_oversample$Urate)
auc(Swiss_ST_rf_multiroc_oversample)
auc(Swiss_ST_rf_caox_roc_oversample)
ci.auc(Swiss_ST_rf_caox_roc_oversample)
auc(Swiss_ST_rf_cap_roc_oversample)
ci.auc(Swiss_ST_rf_cap_roc_oversample)
auc(Swiss_ST_rf_urate_roc_oversample)
ci.auc(Swiss_ST_rf_urate_roc_oversample)


## Partitioning
UK_ST_rpart_predict_oversample <-
  predict(UK_ST_rpart_oversample, newdata = UK_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                              "CaP",
                                                                              "Unknown",
                                                                              "Urate"))
UK_ST_rpart_tb_oversample <-
  table(pred = UK_ST_rpart_predict_oversample,
        ref = UK_ST_non_coded_test1$Stone_type)
confusionMatrix(UK_ST_rpart_tb_oversample)
UK_ST_rpart_tb_oversample
res_UK_ST_rpart <- evalm(UK_ST_rpart_oversample)

Swiss_ST_rpart_predict_oversample <-
  predict(UK_ST_rpart_oversample, newdata = Swiss_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                                 "CaP",
                                                                                 "Unknown",
                                                                                 "Urate"))
Swiss_ST_rpart_predict1_oversample <-
  predict(UK_ST_rpart_oversample, newdata = Swiss_ST_non_coded_test1, type = "prob")

Swiss_ST_rpart_tb_oversample <-
  table(Swiss_ST_rpart_predict_oversample, ref = Swiss_ST_non_coded_test1$Stone_type)
confusionMatrix(Swiss_ST_rpart_tb_oversample)

Swiss_ST_rpart_multiroc_oversample <-
  multiclass.roc(Swiss_ST_non_coded_test1$Stone_type,
                 Swiss_ST_rpart_predict1_oversample)
Swiss_ST_rpart_caox_roc_oversample <- roc(Swiss_non_coded_test_caox,
                               Swiss_ST_rpart_predict1_oversample$CaOx)
Swiss_ST_rpart_cap_roc_oversample <- roc(Swiss_non_coded_test_cap,
                              Swiss_ST_rpart_predict1_oversample$CaP)
Swiss_ST_rpart_urate_roc_oversample <- roc(Swiss_non_coded_test_urate,
                                Swiss_ST_rpart_predict1_oversample$Urate)
auc(Swiss_ST_rpart_multiroc_oversample)
auc(Swiss_ST_rpart_caox_roc_oversample)
ci.auc(Swiss_ST_rpart_caox_roc_oversample)
auc(Swiss_ST_rpart_cap_roc_oversample)
ci.auc(Swiss_ST_rpart_cap_roc_oversample)
auc(Swiss_ST_rpart_urate_roc_oversample)
ci.auc(Swiss_ST_rpart_urate_roc_oversample)

## XGBoost
UK_ST_xgboost_predict_oversample <-
  predict(UK_ST_xgboost_oversample, UK_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                      "CaP",
                                                                      "Unknown",
                                                                      "Urate"))
UK_ST_xgboost_tb_oversample <-
  table(pred = UK_ST_xgboost_predict_oversample, ref = UK_ST_non_coded_test1$Stone_type)
confusionMatrix(UK_ST_xgboost_tb_oversample)
UK_ST_xgboost_tb_oversample
res_UK_ST_xgboost <- evalm(UK_ST_xgboost_oversample)

Swiss_ST_xgboost_predict_oversample <-
  predict(UK_ST_xgboost_oversample, newdata = Swiss_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                                   "CaP",
                                                                                   "Unknown",
                                                                                   "Urate"))
Swiss_ST_xgboost_predict1_oversample <-
  predict(UK_ST_xgboost_oversample, newdata = Swiss_ST_non_coded_test1, type = "prob")

Swiss_ST_xgboost_tb_oversample <-
  table(Swiss_ST_xgboost_predict_oversample, ref = Swiss_ST_non_coded_test1$Stone_type)
confusionMatrix(Swiss_ST_xgboost_tb_oversample)

Swiss_ST_xgboost_multiroc_oversample <-
  multiclass.roc(Swiss_ST_non_coded_test1$Stone_type,
                 Swiss_ST_xgboost_predict1_oversample)
Swiss_ST_xgboost_caox_roc_oversample <- roc(Swiss_non_coded_test_caox,
                                 Swiss_ST_xgboost_predict1_oversample$CaOx)
Swiss_ST_xgboost_cap_roc_oversample <- roc(Swiss_non_coded_test_cap,
                                Swiss_ST_xgboost_predict1_oversample$CaP)
Swiss_ST_xgboost_urate_roc_oversample <- roc(Swiss_non_coded_test_urate,
                                  Swiss_ST_xgboost_predict1_oversample$Urate)
auc(Swiss_ST_xgboost_multiroc_oversample)
auc(Swiss_ST_xgboost_caox_roc_oversample)
ci.auc(Swiss_ST_xgboost_caox_roc_oversample)
auc(Swiss_ST_xgboost_cap_roc_oversample)
ci.auc(Swiss_ST_xgboost_cap_roc_oversample)
auc(Swiss_ST_xgboost_urate_roc_oversample)
ci.auc(Swiss_ST_xgboost_urate_roc_oversample)


## Neural Net
UK_ST_nn_predict_oversample <-
  predict(UK_ST_nn_oversample, UK_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                 "CaP",
                                                                 "Unknown",
                                                                 "Urate"))
UK_ST_nn_tb_oversample <-
  table(pred = UK_ST_nn_predict_oversample,
        ref = UK_ST_non_coded_test1$Stone_type)
confusionMatrix(UK_ST_nn_tb_oversample)
UK_ST_nn_tb_oversample
res_UK_ST_nn <- evalm(UK_ST_nn_oversample)

Swiss_ST_nn_predict_oversample <-
  predict(UK_ST_nn_oversample, newdata = Swiss_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                              "CaP",
                                                                              "Unknown",
                                                                              "Urate"))
Swiss_ST_nn_predict1_oversample <-
  predict(UK_ST_nn_oversample, newdata = Swiss_ST_non_coded_test1, type = "prob")

Swiss_ST_nn_tb_oversample <-
  table(Swiss_ST_nn_predict_oversample, ref = Swiss_ST_non_coded_test1$Stone_type)
confusionMatrix(Swiss_ST_nn_tb_oversample)

Swiss_ST_nn_multiroc_oversample <-
  multiclass.roc(Swiss_ST_non_coded_test1$Stone_type,
                 Swiss_ST_nn_predict1_oversample)
Swiss_ST_nn_caox_roc_oversample <- roc(Swiss_non_coded_test_caox,
                            Swiss_ST_nn_predict1_oversample$CaOx)
Swiss_ST_nn_cap_roc_oversample <- roc(Swiss_non_coded_test_cap,
                           Swiss_ST_nn_predict1_oversample$CaP)
Swiss_ST_nn_urate_roc_oversample <- roc(Swiss_non_coded_test_urate,
                             Swiss_ST_nn_predict1_oversample$Urate)
auc(Swiss_ST_nn_multiroc_oversample)
auc(Swiss_ST_nn_caox_roc_oversample)
ci.auc(Swiss_ST_nn_caox_roc_oversample)
auc(Swiss_ST_nn_cap_roc_oversample)
ci.auc(Swiss_ST_nn_cap_roc_oversample)
auc(Swiss_ST_nn_urate_roc_oversample)
ci.auc(Swiss_ST_nn_urate_roc_oversample)

## Recurrence Deep Neural Net
UK_ST_keras_results_oversample <-
  UK_ST_keras_oversample %>% evaluate(UK_ST_test_coded_predictors2,
                           UK_ST_test_coded_outcome3,
                           verbose = 1)
UK_ST_keras_prediction_oversample <-
  UK_ST_keras_oversample %>% predict(UK_ST_test_coded_predictors2,
                          batch_size = 32,
                          verbose = 1)
UK_ST_keras_prediction1_oversample <- round(UK_ST_keras_prediction_oversample, digits = 0)
UK_ST_keras_tb_oversample <-
  table(UK_ST_test_coded_outcome3, UK_ST_keras_prediction1_oversample)
UK_ST_keras_roc_oversample <-
  roc(UK_ST_test_coded_outcome3, UK_ST_keras_prediction1_oversample)
auc(UK_ST_keras_roc_oversample)
confusionMatrix(UK_ST_keras_tb_oversample)
UK_ST_keras_tb_oversample

Swiss_ST_keras_prediction_oversample <-
  UK_ST_keras_oversample %>% predict(Swiss_ST_test_coded_predictors2,
                          batch_size = 32,
                          verbose = 1)
Swiss_ST_keras_prediction1_oversample <-
  round(Swiss_ST_keras_prediction_oversample, digits = 0)
Swiss_ST_keras_prediction2_oversample <- as_tibble(Swiss_ST_keras_prediction_oversample)
colnames(Swiss_ST_keras_prediction2_oversample) <- c("Unclear",
                                          "CaOx",
                                          "CaP",
                                          "Unknown",
                                          "Urate")
Swiss_ST_keras_prediction2_oversample <- Swiss_ST_keras_prediction2_oversample[, -1]
Swiss_ST_keras_prediction3_oversample <- Swiss_ST_keras_prediction2_oversample
Swiss_ST_keras_prediction3_oversample$CaOx <-
  ifelse(Swiss_ST_keras_prediction3_oversample$CaOx > 0.4, "1", "0") %>% factor(levels = c("0",
                                                                                "1"))
Swiss_ST_keras_prediction3_oversample$CaP <-
  ifelse(Swiss_ST_keras_prediction3_oversample$CaP > 0.4, "1", "0") %>% factor(levels = c("0",
                                                                               "1"))
Swiss_ST_keras_prediction3_oversample$Unknown <-
  ifelse(Swiss_ST_keras_prediction3_oversample$Unknown > 0.4, "1", "0") %>% factor(levels = c("0",
                                                                                   "1"))
Swiss_ST_keras_prediction3_oversample$Urate <-
  ifelse(Swiss_ST_keras_prediction3_oversample$Urate > 0.4, "1", "0") %>% factor(levels = c("0",
                                                                                 "1"))


Swiss_ST_keras_caox_roc_oversample <- roc(Swiss_ST_test_coded_outcome3$CaOx,
                               Swiss_ST_keras_prediction2_oversample$CaOx)
Swiss_ST_keras_cap_roc_oversample <- roc(Swiss_ST_test_coded_outcome3$CaP,
                              Swiss_ST_keras_prediction2_oversample$CaP)
Swiss_ST_keras_urate_roc_oversample <- roc(Swiss_ST_test_coded_outcome3$Urate,
                                Swiss_ST_keras_prediction2_oversample$Urate)
auc(Swiss_ST_nn_multiroc_oversample)
auc(Swiss_ST_nn_caox_roc_oversample)
ci.auc(Swiss_ST_nn_caox_roc_oversample)
auc(Swiss_ST_nn_cap_roc_oversample)
ci.auc(Swiss_ST_nn_cap_roc_oversample)
auc(Swiss_ST_nn_urate_roc_oversample)
ci.auc(Swiss_ST_nn_urate_roc_oversample)

Swiss_ST_keras_tb_oversample <-
  table(
    as.matrix(Swiss_ST_test_coded_outcome3),
    as.matrix(Swiss_ST_keras_prediction3_oversample)
  )
confusionMatrix(Swiss_ST_keras_tb_oversample)


## Sort ROC lists
caox_roc_list_oversample <- list(
  Swiss_ST_rf_caox_roc_oversample,
  Swiss_ST_rpart_caox_roc_oversample,
  Swiss_ST_xgboost_caox_roc_oversample,
  Swiss_ST_nn_caox_roc_oversample,
  Swiss_ST_keras_caox_roc_oversample
)

cap_roc_list_oversample <- list(
  Swiss_ST_rf_cap_roc_oversample,
  Swiss_ST_rpart_cap_roc_oversample,
  Swiss_ST_xgboost_cap_roc_oversample,
  Swiss_ST_nn_cap_roc_oversample,
  Swiss_ST_keras_cap_roc_oversample
)

urate_roc_list_oversample <- list(
  Swiss_ST_rf_urate_roc_oversample,
  Swiss_ST_rpart_urate_roc_oversample,
  Swiss_ST_xgboost_urate_roc_oversample,
  Swiss_ST_nn_urate_roc_oversample,
  Swiss_ST_keras_urate_roc_oversample
)
