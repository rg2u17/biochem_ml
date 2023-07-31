#Create training/test sets:
set.seed(1234)

#Stone type
UK_ST1_coded <- UK_ST1[, -14]
UK_ST1_non_coded <- UK_ST1[, -15]

Swiss_ST_coded <- Swiss_ST[, -14]
Swiss_ST_non_coded <- Swiss_ST[, -15]

UK_ST1_non_coded_sample <-
  sample(1:nrow(UK_ST1_non_coded), size = nrow(UK_ST1_non_coded) * 0.7)
UK_ST_non_coded_train1 <- UK_ST1_non_coded[UK_ST1_non_coded_sample, ]
UK_ST_non_coded_test1 <- UK_ST1_non_coded[-UK_ST1_non_coded_sample, ]
UK_ST_non_coded_train_predictors1 <- UK_ST_non_coded_train1[, -14]
UK_ST_non_coded_test_predictors1 <- UK_ST_non_coded_test1[, -14]
UK_ST_non_coded_train_outcome1 <- UK_ST_non_coded_train1[, -1:-13]
UK_ST_non_coded_test_outcome1 <- UK_ST_non_coded_test1[, -1:-13]

UK_ST1_coded_sample <-
  sample(1:nrow(UK_ST1_coded), size = nrow(UK_ST1_coded) * 0.7)
UK_ST_coded_train1 <- UK_ST1_coded[UK_ST1_coded_sample, ]
UK_ST_coded_test1 <- UK_ST1_coded[-UK_ST1_coded_sample, ]
UK_ST_coded_train_predictors1 <- UK_ST_coded_train1[, -14]
UK_ST_coded_test_predictors1 <- UK_ST_coded_test1[, -14]
UK_ST_coded_train_outcome1 <- UK_ST_coded_train1[, -1:-13]
UK_ST_coded_test_outcome1 <- UK_ST_coded_test1[, -1:-13]

Swiss_ST_non_coded_sample <-
  sample(1:nrow(Swiss_ST_non_coded), size = nrow(Swiss_ST_non_coded) * 0.7)
Swiss_ST_non_coded_train1 <-
  Swiss_ST_non_coded[Swiss_ST_non_coded_sample, ]
Swiss_ST_non_coded_test1 <-
  UK_ST1_non_coded[-Swiss_ST_non_coded_sample, ]
Swiss_ST_non_coded_train_predictors1 <-
  Swiss_ST_non_coded_train1[, -14]
Swiss_ST_non_coded_test_predictors1 <- Swiss_ST_non_coded_test1[, -14]
Swiss_ST_non_coded_train_outcome1 <-
  Swiss_ST_non_coded_train1[, -1:-13]
Swiss_ST_non_coded_test_outcome1 <- Swiss_ST_non_coded_test1[, -1:-13]

Swiss_ST_coded_sample <-
  sample(1:nrow(Swiss_ST_coded), size = nrow(Swiss_ST_coded) * 0.7)
Swiss_ST_coded_train1 <- UK_ST1_coded[Swiss_ST_coded_sample, ]
Swiss_ST_coded_test1 <- UK_ST1_coded[-Swiss_ST_coded_sample, ]
Swiss_ST_coded_train_predictors1 <- Swiss_ST_coded_train1[, -14]
Swiss_ST_coded_test_predictors1 <- Swiss_ST_coded_test1[, -14]
Swiss_ST_coded_train_outcome1 <- Swiss_ST_coded_train1[, -1:-13]
Swiss_ST_coded_test_outcome1 <- Swiss_ST_coded_test1[, -1:-13]

UK_ST_coded_train_predictors3 <-
  UK_ST_coded_train_predictors1[, -11:-13]
UK_ST_coded_test_predictors3 <- UK_ST_coded_test_predictors1[, -11:-13]
UK_ST_coded_train_predictors3$Sex <-
  as.numeric(UK_ST_coded_train_predictors3$Sex)
UK_ST_coded_test_predictors3$Sex <-
  as.numeric(UK_ST_coded_test_predictors3$Sex)
UK_ST_coded_train_predictors3$Age <-
  as.numeric(UK_ST_coded_train_predictors3$Age)
UK_ST_coded_test_predictors3$Age <-
  as.numeric(UK_ST_coded_test_predictors3$Age)
UK_ST_coded_train_predictors3$Urine_vol_L <-
  as.numeric(UK_ST_coded_train_predictors3$Urine_vol_L)
UK_ST_coded_test_predictors3$Urine_vol_L <-
  as.numeric(UK_ST_coded_test_predictors3$Urine_vol_L)
UK_ST_coded_train_predictors3$Urine_Ca_mmol_24h <-
  as.numeric(UK_ST_coded_train_predictors3$Urine_Ca_mmol_24h)
UK_ST_coded_test_predictors3$Urine_Ca_mmol_24h <-
  as.numeric(UK_ST_coded_test_predictors3$Urine_Ca_mmol_24h)
UK_ST_coded_train_predictors3$Urine_ox_mmol_24h <-
  as.numeric(UK_ST_coded_train_predictors3$Urine_ox_mmol_24h)
UK_ST_coded_test_predictors3$Urine_ox_mmol_24h <-
  as.numeric(UK_ST_coded_test_predictors3$Urine_ox_mmol_24h)
UK_ST_coded_train_predictors3$Urine_urate_mmol_24h <-
  as.numeric(UK_ST_coded_train_predictors3$Urine_urate_mmol_24h)
UK_ST_coded_test_predictors3$Urine_urate_mmol_24h <-
  as.numeric(UK_ST_coded_test_predictors3$Urine_urate_mmol_24h)
UK_ST_coded_train_predictors3$pH <-
  as.numeric(UK_ST_coded_train_predictors3$pH)
UK_ST_coded_test_predictors3$pH <-
  as.numeric(UK_ST_coded_test_predictors3$pH)
UK_ST_coded_train_predictors3$Hypercalciuria_coded <-
  as.integer(UK_ST_coded_train_predictors3$Hypercalciuria_coded)
UK_ST_coded_test_predictors3$Hypercalciuria_coded <-
  as.integer(UK_ST_coded_test_predictors3$Hypercalciuria_coded)
UK_ST_coded_train_predictors3$Hyperoxaluria_coded <-
  as.integer(UK_ST_coded_train_predictors3$Hyperoxaluria_coded)
UK_ST_coded_test_predictors3$Hyperoxaluria_coded <-
  as.integer(UK_ST_coded_test_predictors3$Hyperoxaluria_coded)
UK_ST_coded_train_predictors3$Hyperuricosuria_coded <-
  as.integer(UK_ST_coded_train_predictors3$Hyperuricosuria_coded)
UK_ST_coded_test_predictors3$Hyperuricosuria_coded <-
  as.integer(UK_ST_coded_test_predictors3$Hyperuricosuria_coded)


UK_ST_train_coded_predictors2 <-
  as.matrix(UK_ST_coded_train_predictors3)
UK_ST_train_coded_outcome2 <- as.matrix(UK_ST_coded_train_outcome1)
UK_ST_train_coded_outcome3 <-
  to_categorical(UK_ST_train_coded_outcome2)
UK_ST_test_coded_predictors2 <-
  as.matrix(UK_ST_coded_test_predictors3)
UK_ST_test_coded_outcome2 <- as.matrix(UK_ST_coded_test_outcome1)
UK_ST_test_coded_outcome3 <- to_categorical(UK_ST_test_coded_outcome2)

Swiss_ST_coded_train_predictors3 <-
  Swiss_ST_coded_train_predictors1[, -11:-13]
Swiss_ST_coded_test_predictors3 <-
  Swiss_ST_coded_test_predictors1[, -11:-13]
Swiss_ST_coded_train_predictors3$Sex <-
  as.integer(Swiss_ST_coded_train_predictors3$Sex)
Swiss_ST_coded_test_predictors3$Sex <-
  as.integer(Swiss_ST_coded_test_predictors3$Sex)

Swiss_ST_train_coded_predictors2 <-
  as.matrix(Swiss_ST_coded_train_predictors3)
Swiss_ST_train_coded_outcome2 <-
  as.matrix(Swiss_ST_coded_train_outcome1)
Swiss_ST_train_coded_outcome3 <-
  to_categorical(Swiss_ST_train_coded_outcome2)
Swiss_ST_test_coded_predictors2 <-
  as.matrix(Swiss_ST_coded_test_predictors3)
Swiss_ST_test_coded_outcome2 <-
  as.matrix(Swiss_ST_coded_test_outcome1)
Swiss_ST_test_coded_outcome3 <-
  to_categorical(Swiss_ST_test_coded_outcome2)

#Recurrence
UK_Rec5 <- UK_Rec[, -14:-17]
UK_Rec4 <- UK_Rec[, -1:-13]
UK_Rec3 <- UK_Rec4[, -1:-2]
UK_Rec2 <- UK_Rec4[, -3:-4]
UK_Rec1 <- cbind(UK_Rec5, UK_Rec3, UK_Rec2)

Swiss_Rec5 <- Swiss_Rec[, -14:-17]
Swiss_Rec4 <- Swiss_Rec[, -1:-13]
Swiss_Rec3 <- Swiss_Rec4[, -1:-2]
Swiss_Rec2 <- Swiss_Rec4[, -3:-4]
Swiss_Rec1 <- cbind(Swiss_Rec5, Swiss_Rec3, Swiss_Rec2)

UK_Rec_coded <- UK_Rec1[, -16]
UK_Rec_non_coded <- UK_Rec1[, -17]

Swiss_Rec_coded <- Swiss_Rec1[, -16]
Swiss_Rec_non_coded <- Swiss_Rec1[, -17]

UK_Rec_non_coded_sample <-
  sample(1:nrow(UK_Rec_non_coded), size = nrow(UK_Rec_non_coded) * 0.7)
UK_Rec_non_coded_train1 <- UK_Rec_non_coded[UK_Rec_non_coded_sample, ]
UK_Rec_non_coded_test1 <- UK_Rec_non_coded[-UK_Rec_non_coded_sample, ]
UK_Rec_non_coded_train_predictors1 <- UK_Rec_non_coded_train1[, -16]
UK_Rec_non_coded_test_predictors1 <- UK_Rec_non_coded_test1[, -16]
UK_Rec_non_coded_train_outcome1 <- UK_Rec_non_coded_train1[, -1:-15]
UK_Rec_non_coded_test_outcome1 <- UK_Rec_non_coded_test1[, -1:-15]

UK_Rec_coded_sample <-
  sample(1:nrow(UK_Rec_coded), size = nrow(UK_Rec_coded) * 0.7)
UK_Rec_coded_train1 <- UK_Rec_coded[UK_Rec_coded_sample, ]
UK_Rec_coded_test1 <- UK_Rec_coded[-UK_Rec_coded_sample, ]
UK_Rec_coded_train_predictors1 <- UK_Rec_coded_train1[, -16]
UK_Rec_coded_test_predictors1 <- UK_Rec_coded_test1[, -16]
UK_Rec_coded_train_outcome1 <- UK_Rec_coded_train1[, -1:-15]
UK_Rec_coded_test_outcome1 <- UK_Rec_coded_test1[, -1:-15]

UK_Rec_coded_train_predictors3 <-
  UK_Rec_coded_train_predictors1[, -11:-14]
UK_Rec_coded_test_predictors3 <-
  UK_Rec_coded_test_predictors1[, -11:-14]
UK_Rec_coded_train_predictors3$Sex <-
  as.integer(UK_Rec_coded_train_predictors3$Sex)
UK_Rec_coded_test_predictors3$Sex <-
  as.integer(UK_Rec_coded_test_predictors3$Sex)

Swiss_Rec_non_coded_sample <-
  sample(1:nrow(Swiss_Rec_non_coded), size = nrow(Swiss_Rec_non_coded) *
           0.7)
Swiss_Rec_non_coded_train1 <-
  Swiss_Rec_non_coded[Swiss_Rec_non_coded_sample, ]
Swiss_Rec_non_coded_test1 <-
  Swiss_Rec_non_coded[-Swiss_Rec_non_coded_sample, ]
Swiss_Rec_non_coded_train_predictors1 <-
  Swiss_Rec_non_coded_train1[, -16]
Swiss_Rec_non_coded_test_predictors1 <-
  Swiss_Rec_non_coded_test1[, -16]
Swiss_Rec_non_coded_train_outcome1 <-
  Swiss_Rec_non_coded_train1[, -1:-15]
Swiss_Rec_non_coded_test_outcome1 <-
  Swiss_Rec_non_coded_test1[, -1:-15]

Swiss_Rec_coded_sample <-
  sample(1:nrow(Swiss_Rec_coded), size = nrow(UK_Rec_coded) * 0.7)
Swiss_Rec_coded_train1 <- Swiss_Rec_coded[Swiss_Rec_coded_sample, ]
Swiss_Rec_coded_test1 <- Swiss_Rec_coded[-Swiss_Rec_coded_sample, ]
Swiss_Rec_coded_train_predictors1 <- Swiss_Rec_coded_train1[, -16]
Swiss_Rec_coded_test_predictors1 <- Swiss_Rec_coded_test1[, -16]
Swiss_Rec_coded_train_outcome1 <- Swiss_Rec_coded_train1[, -1:-15]
Swiss_Rec_coded_test_outcome1 <- Swiss_Rec_coded_test1[, -1:-15]

Swiss_Rec_coded_train_predictors3 <-
  Swiss_Rec_coded_train_predictors1[, -11:-14]
Swiss_Rec_coded_test_predictors3 <-
  Swiss_Rec_coded_test_predictors1[, -11:-14]
Swiss_Rec_coded_train_predictors3$Sex <-
  as.integer(Swiss_Rec_coded_train_predictors3$Sex)
Swiss_Rec_coded_test_predictors3$Sex <-
  as.integer(Swiss_Rec_coded_test_predictors3$Sex)

UK_Rec_train_coded_predictors2 <-
  as.matrix(UK_Rec_coded_train_predictors3)
UK_Rec_train_coded_outcome2 <- as.numeric(UK_Rec_coded_train_outcome1)
UK_Rec_test_coded_predictors2 <-
  as.matrix(UK_Rec_coded_test_predictors3)
UK_Rec_test_coded_outcome2 <- as.numeric(UK_Rec_coded_test_outcome1)

Swiss_Rec_train_coded_predictors2 <-
  as.matrix(Swiss_Rec_coded_train_predictors3)
Swiss_Rec_train_coded_outcome2 <-
  as.numeric(Swiss_Rec_coded_train_outcome1)
Swiss_Rec_test_coded_predictors2 <-
  as.matrix(Swiss_Rec_coded_test_predictors3)
Swiss_Rec_test_coded_outcome2 <-
  as.numeric(Swiss_Rec_coded_test_outcome1)