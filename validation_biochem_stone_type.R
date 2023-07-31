# Stone Type
UK_non_coded_test_caox<-ifelse(UK_ST_non_coded_test1$Stone_type=="CaOx", 1, 0) %>% factor(levels = c("0",
                                                                                                     "1"))
UK_non_coded_test_cap<-ifelse(UK_ST_non_coded_test1$Stone_type=="CaP", 1, 0) %>% factor(levels = c("0",
                                                                                                     "1"))
UK_non_coded_test_urate<-ifelse(UK_ST_non_coded_test1$Stone_type=="Urate", 1, 0) %>% factor(levels = c("0",
                                                                                                     "1"))

Swiss_non_coded_test_caox<-ifelse(Swiss_ST_non_coded_test1$Stone_type=="CaOx", 1, 0) %>% factor(levels = c("0",
                                                                                                     "1"))
Swiss_non_coded_test_cap<-ifelse(Swiss_ST_non_coded_test1$Stone_type=="CaP", 1, 0) %>% factor(levels = c("0",
                                                                                                   "1"))
Swiss_non_coded_test_urate<-ifelse(Swiss_ST_non_coded_test1$Stone_type=="Urate", 1, 0) %>% factor(levels = c("0",
                                                                                                       "1"))

UK_ST_non_coded_test1$Stone_type <- factor(UK_ST_non_coded_test1$Stone_type,
                                            levels = c("CaOx",
                                                       "CaP",
                                                       "Unknown",
                                                       "Urate"))
Swiss_ST_non_coded_test1$Stone_type <- factor(Swiss_ST_non_coded_test1$Stone_type,
                                           levels = c("CaOx",
                                                      "CaP",
                                                      "Unknown",
                                                      "Urate"))

Swiss_ST_test_coded_outcome3 <- Swiss_ST_test_coded_outcome3[,-1] %>% as_tibble()
colnames(Swiss_ST_test_coded_outcome3) <- c("CaOx",
                                            "CaP",
                                            "Unknown",
                                            "Urate")
Swiss_ST_test_coded_outcome3$CaOx <-
  round(Swiss_ST_test_coded_outcome3$CaOx, digits = 0) %>% factor(levels = c("0",
                                                                           "1"))
Swiss_ST_test_coded_outcome3$CaP <-
  round(Swiss_ST_test_coded_outcome3$CaP, digits = 0) %>% factor(levels = c("0",
                                                                          "1"))
Swiss_ST_test_coded_outcome3$Unknown <-
  round(Swiss_ST_test_coded_outcome3$Unknown, digits = 0) %>% factor(levels = c("0",
                                                                              "1"))
Swiss_ST_test_coded_outcome3$Urate <-
  round(Swiss_ST_test_coded_outcome3$Urate, digits = 0) %>% factor(levels = c("0",
                                                                            "1"))


## Random Forests
UK_ST_rf_predict <- predict(UK_ST_rf, newdata = UK_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                                             "CaP",
                                                                                             "Unknown",
                                                                                             "Urate"))
UK_ST_rf_predict1 <- predict(UK_ST_rf, newdata = UK_ST_non_coded_test1, type = "prob")
UK_ST_rf_tb <-
  table(UK_ST_rf_predict, ref = UK_ST_non_coded_test1$Stone_type)
confusionMatrix(UK_ST_rf_tb)
res_UK_ST_rf <- evalm(UK_ST_rf)

Swiss_ST_rf_predict <- predict(UK_ST_rf, newdata = Swiss_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                                                   "CaP",
                                                                                                   "Unknown",
                                                                                                   "Urate"))
Swiss_ST_rf_predict1 <- predict(UK_ST_rf, newdata = Swiss_ST_non_coded_test1, type = "prob")

Swiss_ST_rf_tb <-
  table(Swiss_ST_rf_predict, ref = Swiss_ST_non_coded_test1$Stone_type)
confusionMatrix(Swiss_ST_rf_tb)

Swiss_ST_rf_multiroc <- multiclass.roc(Swiss_ST_non_coded_test1$Stone_type,
                        Swiss_ST_rf_predict1)
Swiss_ST_rf_caox_roc <- roc(Swiss_non_coded_test_caox,
                        Swiss_ST_rf_predict1$CaOx)
Swiss_ST_rf_cap_roc <- roc(Swiss_non_coded_test_cap,
                        Swiss_ST_rf_predict1$CaP)
Swiss_ST_rf_urate_roc <- roc(Swiss_non_coded_test_urate,
                        Swiss_ST_rf_predict1$Urate)
auc(Swiss_ST_rf_multiroc)
auc(Swiss_ST_rf_caox_roc)
ci.auc(Swiss_ST_rf_caox_roc)
auc(Swiss_ST_rf_cap_roc)
ci.auc(Swiss_ST_rf_cap_roc)
auc(Swiss_ST_rf_urate_roc)
ci.auc(Swiss_ST_rf_urate_roc)


## Partitioning
UK_ST_rpart_predict <-
  predict(UK_ST_rpart, newdata = UK_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                              "CaP",
                                                                              "Unknown",
                                                                              "Urate"))
UK_ST_rpart_tb <-
  table(pred = UK_ST_rpart_predict,
        ref = UK_ST_non_coded_test1$Stone_type)
confusionMatrix(UK_ST_rpart_tb)
UK_ST_rpart_tb
res_UK_ST_rpart <- evalm(UK_ST_rpart)

Swiss_ST_rpart_predict <- predict(UK_ST_rpart, newdata = Swiss_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                                                         "CaP",
                                                                                                         "Unknown",
                                                                                                         "Urate"))
Swiss_ST_rpart_predict1 <- predict(UK_ST_rpart, newdata = Swiss_ST_non_coded_test1, type = "prob")

Swiss_ST_rpart_tb <-
  table(Swiss_ST_rpart_predict, ref = Swiss_ST_non_coded_test1$Stone_type)
confusionMatrix(Swiss_ST_rpart_tb)

Swiss_ST_rpart_multiroc <- multiclass.roc(Swiss_ST_non_coded_test1$Stone_type,
                                       Swiss_ST_rpart_predict1)
Swiss_ST_rpart_caox_roc <- roc(Swiss_non_coded_test_caox,
                            Swiss_ST_rpart_predict1$CaOx)
Swiss_ST_rpart_cap_roc <- roc(Swiss_non_coded_test_cap,
                           Swiss_ST_rpart_predict1$CaP)
Swiss_ST_rpart_urate_roc <- roc(Swiss_non_coded_test_urate,
                             Swiss_ST_rpart_predict1$Urate)
auc(Swiss_ST_rpart_multiroc)
auc(Swiss_ST_rpart_caox_roc)
ci.auc(Swiss_ST_rpart_caox_roc)
auc(Swiss_ST_rpart_cap_roc)
ci.auc(Swiss_ST_rpart_cap_roc)
auc(Swiss_ST_rpart_urate_roc)
ci.auc(Swiss_ST_rpart_urate_roc)

## XGBoost
UK_ST_xgboost_predict <-
  predict(UK_ST_xgboost, UK_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                      "CaP",
                                                                      "Unknown",
                                                                      "Urate"))
UK_ST_xgboost_tb <-
  table(pred = UK_ST_xgboost_predict, ref = UK_ST_non_coded_test1$Stone_type)
confusionMatrix(UK_ST_xgboost_tb)
UK_ST_xgboost_tb
res_UK_ST_xgboost <- evalm(UK_ST_xgboost)

Swiss_ST_xgboost_predict <- predict(UK_ST_xgboost, newdata = Swiss_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                                                         "CaP",
                                                                                                         "Unknown",
                                                                                                         "Urate"))
Swiss_ST_xgboost_predict1 <- predict(UK_ST_xgboost, newdata = Swiss_ST_non_coded_test1, type = "prob")

Swiss_ST_xgboost_tb <-
  table(Swiss_ST_xgboost_predict, ref = Swiss_ST_non_coded_test1$Stone_type)
confusionMatrix(Swiss_ST_xgboost_tb)

Swiss_ST_xgboost_multiroc <- multiclass.roc(Swiss_ST_non_coded_test1$Stone_type,
                                          Swiss_ST_xgboost_predict1)
Swiss_ST_xgboost_caox_roc <- roc(Swiss_non_coded_test_caox,
                               Swiss_ST_xgboost_predict1$CaOx)
Swiss_ST_xgboost_cap_roc <- roc(Swiss_non_coded_test_cap,
                              Swiss_ST_xgboost_predict1$CaP)
Swiss_ST_xgboost_urate_roc <- roc(Swiss_non_coded_test_urate,
                                Swiss_ST_xgboost_predict1$Urate)
auc(Swiss_ST_xgboost_multiroc)
auc(Swiss_ST_xgboost_caox_roc)
ci.auc(Swiss_ST_xgboost_caox_roc)
auc(Swiss_ST_xgboost_cap_roc)
ci.auc(Swiss_ST_xgboost_cap_roc)
auc(Swiss_ST_xgboost_urate_roc)
ci.auc(Swiss_ST_xgboost_urate_roc)


## Neural Net
UK_ST_nn_predict <- predict(UK_ST_nn, UK_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                                   "CaP",
                                                                                   "Unknown",
                                                                                   "Urate"))
UK_ST_nn_tb <-
  table(pred = UK_ST_nn_predict,
        ref = UK_ST_non_coded_test1$Stone_type)
confusionMatrix(UK_ST_nn_tb)
UK_ST_nn_tb
res_UK_ST_nn <- evalm(UK_ST_nn)

Swiss_ST_nn_predict <- predict(UK_ST_nn, newdata = Swiss_ST_non_coded_test1) %>% factor(levels = c("CaOx",
                                                                                                         "CaP",
                                                                                                         "Unknown",
                                                                                                         "Urate"))
Swiss_ST_nn_predict1 <- predict(UK_ST_nn, newdata = Swiss_ST_non_coded_test1, type = "prob")

Swiss_ST_nn_tb <-
  table(Swiss_ST_nn_predict, ref = Swiss_ST_non_coded_test1$Stone_type)
confusionMatrix(Swiss_ST_nn_tb)

Swiss_ST_nn_multiroc <- multiclass.roc(Swiss_ST_non_coded_test1$Stone_type,
                                          Swiss_ST_nn_predict1)
Swiss_ST_nn_caox_roc <- roc(Swiss_non_coded_test_caox,
                               Swiss_ST_nn_predict1$CaOx)
Swiss_ST_nn_cap_roc <- roc(Swiss_non_coded_test_cap,
                              Swiss_ST_nn_predict1$CaP)
Swiss_ST_nn_urate_roc <- roc(Swiss_non_coded_test_urate,
                                Swiss_ST_nn_predict1$Urate)
auc(Swiss_ST_nn_multiroc)
auc(Swiss_ST_nn_caox_roc)
ci.auc(Swiss_ST_nn_caox_roc)
auc(Swiss_ST_nn_cap_roc)
ci.auc(Swiss_ST_nn_cap_roc)
auc(Swiss_ST_nn_urate_roc)
ci.auc(Swiss_ST_nn_urate_roc)

## Recurrence Deep Neural Net
UK_ST_keras_results <-
  UK_ST_keras %>% evaluate(UK_ST_test_coded_predictors2,
                           UK_ST_test_coded_outcome3,
                           verbose = 1)
UK_ST_keras_prediction <-
  UK_ST_keras %>% predict(UK_ST_test_coded_predictors2,
                          batch_size = 32,
                          verbose = 1)
UK_ST_keras_prediction1 <- round(UK_ST_keras_prediction, digits = 0)
UK_ST_keras_tb <-
  table(UK_ST_test_coded_outcome3, UK_ST_keras_prediction1)
UK_ST_keras_roc <-
  roc(UK_ST_test_coded_outcome3, UK_ST_keras_prediction1)
auc(UK_ST_keras_roc)
confusionMatrix(UK_ST_keras_tb)
UK_ST_keras_tb

Swiss_ST_keras_prediction <-
  UK_ST_keras %>% predict(Swiss_ST_test_coded_predictors2,
                          batch_size = 32,
                          verbose = 1)
Swiss_ST_keras_prediction1 <-
  round(Swiss_ST_keras_prediction, digits = 0)
Swiss_ST_keras_prediction2 <- as_tibble(Swiss_ST_keras_prediction)
colnames(Swiss_ST_keras_prediction2) <- c("Unclear",
                                          "CaOx",
                                          "CaP",
                                          "Unknown",
                                          "Urate")
Swiss_ST_keras_prediction2<-Swiss_ST_keras_prediction2[,-1]
Swiss_ST_keras_prediction3 <- Swiss_ST_keras_prediction2
Swiss_ST_keras_prediction3$CaOx<-ifelse(Swiss_ST_keras_prediction3$CaOx>0.4, "1", "0") %>% factor(levels = c("0",
                                                                                                          "1"))
Swiss_ST_keras_prediction3$CaP<-ifelse(Swiss_ST_keras_prediction3$CaP>0.4, "1", "0") %>% factor(levels = c("0",
                                                                                                          "1"))
Swiss_ST_keras_prediction3$Unknown<-ifelse(Swiss_ST_keras_prediction3$Unknown>0.4, "1", "0") %>% factor(levels = c("0",
                                                                                                          "1"))
Swiss_ST_keras_prediction3$Urate<-ifelse(Swiss_ST_keras_prediction3$Urate>0.4, "1", "0") %>% factor(levels = c("0",
                                                                                                          "1"))


Swiss_ST_keras_caox_roc <- roc(Swiss_ST_test_coded_outcome3$CaOx,
                               Swiss_ST_keras_prediction2$CaOx)
Swiss_ST_keras_cap_roc <- roc(Swiss_ST_test_coded_outcome3$CaP,
                              Swiss_ST_keras_prediction2$CaP)
Swiss_ST_keras_urate_roc <- roc(Swiss_ST_test_coded_outcome3$Urate,
                                Swiss_ST_keras_prediction2$Urate)
auc(Swiss_ST_nn_multiroc)
auc(Swiss_ST_nn_caox_roc)
ci.auc(Swiss_ST_nn_caox_roc)
auc(Swiss_ST_nn_cap_roc)
ci.auc(Swiss_ST_nn_cap_roc)
auc(Swiss_ST_nn_urate_roc)
ci.auc(Swiss_ST_nn_urate_roc)


## Sort ROC lists
caox_roc_list<-list(Swiss_ST_rf_caox_roc,
                    Swiss_ST_rpart_caox_roc,
                    Swiss_ST_xgboost_caox_roc,
                    Swiss_ST_nn_caox_roc,
                    Swiss_ST_keras_caox_roc)
  
cap_roc_list<-list(Swiss_ST_rf_cap_roc,
                     Swiss_ST_rpart_cap_roc,
                     Swiss_ST_xgboost_cap_roc,
                     Swiss_ST_nn_cap_roc,
                     Swiss_ST_keras_cap_roc)
  
urate_roc_list<-list(Swiss_ST_rf_urate_roc,
                       Swiss_ST_rpart_urate_roc,
                       Swiss_ST_xgboost_urate_roc,
                       Swiss_ST_nn_urate_roc,
                       Swiss_ST_keras_urate_roc)
