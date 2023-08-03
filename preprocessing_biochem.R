library(tidyverse)
library(dplyr)
library(DataExplorer)
library(janitor)
library(targets)
library(naniar)
library(qdapTools)
library(DataExplorer)
library(tidyverse)
library(tidymodels)
library(mice)
library(arm)
library(keras)
library(tensorflow)
library(MLeval)
library(pROC)
library(caret)
library(tidymodels)
library(gt)
library(gtExtras)
library(tensorflow)
library(ROSE)

#Create training/test sets:
set.seed(1234)

#Stone type
UK_ST1_coded <- UK_ST1[,-14]
UK_ST1_non_coded <- UK_ST1[,-15]

Swiss_ST_coded <- Swiss_ST[,-14]
Swiss_ST_non_coded <- Swiss_ST[,-15]

UK_ST1_non_coded_sample <-
  sample(1:nrow(UK_ST1_non_coded), size = nrow(UK_ST1_non_coded) * 0.7)
UK_ST_non_coded_train1 <-
  UK_ST1_non_coded[UK_ST1_non_coded_sample,]
UK_ST_non_coded_test1 <-
  UK_ST1_non_coded[-UK_ST1_non_coded_sample,]
UK_ST_non_coded_train_predictors1 <- UK_ST_non_coded_train1[,-14]
UK_ST_non_coded_test_predictors1 <- UK_ST_non_coded_test1[,-14]
UK_ST_non_coded_train_outcome1 <- UK_ST_non_coded_train1[,-1:-13]
UK_ST_non_coded_test_outcome1 <- UK_ST_non_coded_test1[,-1:-13]

UK_ST1_coded_sample <-
  sample(1:nrow(UK_ST1_coded), size = nrow(UK_ST1_coded) * 0.7)
UK_ST_coded_train1 <- UK_ST1_coded[UK_ST1_coded_sample,]
UK_ST_coded_test1 <- UK_ST1_coded[-UK_ST1_coded_sample,]
UK_ST_coded_train_predictors1 <- UK_ST_coded_train1[,-14]
UK_ST_coded_test_predictors1 <- UK_ST_coded_test1[,-14]
UK_ST_coded_train_outcome1 <- UK_ST_coded_train1[,-1:-13]
UK_ST_coded_test_outcome1 <- UK_ST_coded_test1[,-1:-13]

Swiss_ST_non_coded_sample <-
  sample(1:nrow(Swiss_ST_non_coded), size = nrow(Swiss_ST_non_coded) * 0.7)
Swiss_ST_non_coded_train1 <-
  Swiss_ST_non_coded[Swiss_ST_non_coded_sample,]
Swiss_ST_non_coded_test1 <-
  UK_ST1_non_coded[-Swiss_ST_non_coded_sample,]
Swiss_ST_non_coded_train_predictors1 <-
  Swiss_ST_non_coded_train1[,-14]
Swiss_ST_non_coded_test_predictors1 <-
  Swiss_ST_non_coded_test1[,-14]
Swiss_ST_non_coded_train_outcome1 <-
  Swiss_ST_non_coded_train1[,-1:-13]
Swiss_ST_non_coded_test_outcome1 <-
  Swiss_ST_non_coded_test1[,-1:-13]

Swiss_ST_coded_sample <-
  sample(1:nrow(Swiss_ST_coded), size = nrow(Swiss_ST_coded) * 0.7)
Swiss_ST_coded_train1 <- UK_ST1_coded[Swiss_ST_coded_sample,]
Swiss_ST_coded_test1 <- UK_ST1_coded[-Swiss_ST_coded_sample,]
Swiss_ST_coded_train_predictors1 <- Swiss_ST_coded_train1[,-14]
Swiss_ST_coded_test_predictors1 <- Swiss_ST_coded_test1[,-14]
Swiss_ST_coded_train_outcome1 <- Swiss_ST_coded_train1[,-1:-13]
Swiss_ST_coded_test_outcome1 <- Swiss_ST_coded_test1[,-1:-13]

UK_ST_coded_train_predictors3 <-
  UK_ST_coded_train_predictors1[,-11:-13]
UK_ST_coded_test_predictors3 <-
  UK_ST_coded_test_predictors1[,-11:-13]
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
UK_ST_test_coded_outcome3 <-
  to_categorical(UK_ST_test_coded_outcome2)

Swiss_ST_coded_train_predictors3 <-
  Swiss_ST_coded_train_predictors1[,-11:-13]
Swiss_ST_coded_test_predictors3 <-
  Swiss_ST_coded_test_predictors1[,-11:-13]
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
UK_Rec5 <- UK_Rec[,-14:-17]
UK_Rec4 <- UK_Rec[,-1:-13]
UK_Rec3 <- UK_Rec4[,-1:-2]
UK_Rec2 <- UK_Rec4[,-3:-4]
UK_Rec1 <- cbind(UK_Rec5, UK_Rec3, UK_Rec2)

Swiss_Rec5 <- Swiss_Rec[,-14:-17]
Swiss_Rec4 <- Swiss_Rec[,-1:-13]
Swiss_Rec3 <- Swiss_Rec4[,-1:-2]
Swiss_Rec2 <- Swiss_Rec4[,-3:-4]
Swiss_Rec1 <- cbind(Swiss_Rec5, Swiss_Rec3, Swiss_Rec2)

UK_Rec_coded <- UK_Rec1[,-16]
UK_Rec_non_coded <- UK_Rec1[,-17]

Swiss_Rec_coded <- Swiss_Rec1[,-16]
Swiss_Rec_non_coded <- Swiss_Rec1[,-17]

UK_Rec_non_coded_sample <-
  sample(1:nrow(UK_Rec_non_coded), size = nrow(UK_Rec_non_coded) * 0.7)
UK_Rec_non_coded_train1 <-
  UK_Rec_non_coded[UK_Rec_non_coded_sample,]
UK_Rec_non_coded_test1 <-
  UK_Rec_non_coded[-UK_Rec_non_coded_sample,]
UK_Rec_non_coded_train_predictors1 <- UK_Rec_non_coded_train1[,-16]
UK_Rec_non_coded_test_predictors1 <- UK_Rec_non_coded_test1[,-16]
UK_Rec_non_coded_train_outcome1 <- UK_Rec_non_coded_train1[,-1:-15]
UK_Rec_non_coded_test_outcome1 <- UK_Rec_non_coded_test1[,-1:-15]

UK_Rec_coded_sample <-
  sample(1:nrow(UK_Rec_coded), size = nrow(UK_Rec_coded) * 0.7)
UK_Rec_coded_train1 <- UK_Rec_coded[UK_Rec_coded_sample,]
UK_Rec_coded_test1 <- UK_Rec_coded[-UK_Rec_coded_sample,]
UK_Rec_coded_train_predictors1 <- UK_Rec_coded_train1[,-16]
UK_Rec_coded_test_predictors1 <- UK_Rec_coded_test1[,-16]
UK_Rec_coded_train_outcome1 <- UK_Rec_coded_train1[,-1:-15]
UK_Rec_coded_test_outcome1 <- UK_Rec_coded_test1[,-1:-15]

UK_Rec_coded_train_predictors3 <-
  UK_Rec_coded_train_predictors1[,-11:-14]
UK_Rec_coded_test_predictors3 <-
  UK_Rec_coded_test_predictors1[,-11:-14]
UK_Rec_coded_train_predictors3$Sex <-
  as.integer(UK_Rec_coded_train_predictors3$Sex)
UK_Rec_coded_test_predictors3$Sex <-
  as.integer(UK_Rec_coded_test_predictors3$Sex)

Swiss_Rec_non_coded_sample <-
  sample(1:nrow(Swiss_Rec_non_coded), size = nrow(Swiss_Rec_non_coded) *
           0.7)
Swiss_Rec_non_coded_train1 <-
  Swiss_Rec_non_coded[Swiss_Rec_non_coded_sample,]
Swiss_Rec_non_coded_test1 <-
  Swiss_Rec_non_coded[-Swiss_Rec_non_coded_sample,]
Swiss_Rec_non_coded_train_predictors1 <-
  Swiss_Rec_non_coded_train1[,-16]
Swiss_Rec_non_coded_train_outcome1 <-
  Swiss_Rec_non_coded_train1[,-1:-15]

#### Change test set to entire dataset for external validation

Swiss_Rec_non_coded_test_predictors1 <-
  Swiss_Rec_non_coded[,-16]
Swiss_Rec_non_coded_test_outcome1 <-
  Swiss_Rec_non_coded[,-1:-15]

Swiss_Rec_coded_sample <-
  sample(1:nrow(Swiss_Rec_coded), size = nrow(UK_Rec_coded) * 0.7)
Swiss_Rec_coded_train1 <- Swiss_Rec_coded[Swiss_Rec_coded_sample,]
Swiss_Rec_coded_test1 <- Swiss_Rec_coded[-Swiss_Rec_coded_sample,]
Swiss_Rec_coded_train_predictors1 <- Swiss_Rec_coded_train1[,-16]
Swiss_Rec_coded_train_outcome1 <- Swiss_Rec_coded_train1[,-1:-15]

#### Change test set to entire dataset for external validation

Swiss_Rec_coded_test_predictors1 <- Swiss_Rec_coded[,-16]
Swiss_Rec_coded_test_outcome1 <- Swiss_Rec_coded[,-1:-15]

Swiss_Rec_coded_train_predictors3 <-
  Swiss_Rec_coded_train_predictors1[,-11:-14]
Swiss_Rec_coded_test_predictors3 <-
  Swiss_Rec_coded_test_predictors1[,-11:-14]
Swiss_Rec_coded_train_predictors3$Sex <-
  as.integer(Swiss_Rec_coded_train_predictors3$Sex)
Swiss_Rec_coded_test_predictors3$Sex <-
  as.integer(Swiss_Rec_coded_test_predictors3$Sex)

UK_Rec_train_coded_predictors2 <-
  as.matrix(UK_Rec_coded_train_predictors3)
UK_Rec_train_coded_outcome2 <-
  as.numeric(UK_Rec_coded_train_outcome1)
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

# Impute
## Stone Type
UK_ST$Hyperuricosuria <- factor(UK_ST$Hyperuricosuria,
                                levels = c("No",
                                           "Yes"))
UK_ST$Stone.Coded <- as.factor(UK_ST$Stone.Coded)
UK_ST1_coded <- UK_ST[, -14] %>% drop_na(Stone.Coded)

UK_ST1_coded_imputed1 <- mice(UK_ST1_coded,
                              m=5)
UK_ST1_coded_imputed <- complete(UK_ST1_coded_imputed1)
UK_ST1_coded_imputed <-  subset(UK_ST1_coded_imputed, select=-c(Hyperuricosuria,
                                                           Hyperoxaluria,
                                                           Hypercalciuria)) %>% na.omit()

UK_ST1_non_coded$Stone_type <-
  factor(UK_ST1_non_coded$Stone_type,
         levels = c("CaOx",
                    "CaP",
                    "Unknown",
                    "Urate"))
UK_ST1_non_coded_imputed1 <- mice(UK_ST1_non_coded,
                                 m=5)
UK_ST1_non_coded_imputed <- complete(UK_ST1_non_coded_imputed1)
UK_ST1_non_coded_imputed <- UK_ST1_non_coded_imputed %>% select(-Hyperuricosuria)
UK_ST1_non_coded_imputed <-
  UK_ST1_non_coded_imputed %>% mutate(Hyperuricosuria = ifelse(Urine_urate_mmol_24h >
                                                                 4.46, "Yes", "No")) %>% na.omit()
UK_ST1_non_coded_imputed$Hyperuricosuria <-
  factor(UK_ST1_non_coded_imputed$Hyperuricosuria,
         levels = c("Yes",
                    "No"))
UK_ST1_non_coded_imputed <- na.omit(UK_ST1_non_coded_imputed)

UK_ST1_coded_sample_imputed <-
  sample(1:nrow(UK_ST1_coded_imputed),
         size = nrow(UK_ST1_coded_imputed) * 0.7)
UK_ST_coded_train1_imputed <-
  UK_ST1_coded_imputed[UK_ST1_coded_sample_imputed, ]
UK_ST_coded_test1_imputed <-
  UK_ST1_coded_imputed[-UK_ST1_coded_sample_imputed, ]
UK_ST_coded_train_predictors1_imputed <-
  subset(UK_ST_coded_train1_imputed, select = -Stone.Coded)
UK_ST_coded_test_predictors1_imputed <-
  subset(UK_ST_coded_test1_imputed, select = -Stone.Coded)
UK_ST_coded_train_outcome1_imputed <-
  subset(UK_ST_coded_train1_imputed, select = Stone.Coded)
UK_ST_coded_test_outcome1_imputed <-
  subset(UK_ST_coded_test1_imputed, select = Stone.Coded)

UK_ST1_non_coded_sample_imputed <-
  sample(1:nrow(UK_ST1_non_coded_imputed),
         size = nrow(UK_ST1_non_coded_imputed) * 0.7)
UK_ST_non_coded_train1_imputed <-
  UK_ST1_non_coded_imputed[UK_ST1_non_coded_sample_imputed, ]
UK_ST_non_coded_test1_imputed <-
  UK_ST1_non_coded[-UK_ST1_non_coded_sample_imputed, ]
UK_ST_non_coded_train_predictors1_imputed <-
  subset(UK_ST_non_coded_train1_imputed, select = -Stone_type)
UK_ST_non_coded_test_predictors1_imputed <-
  subset(UK_ST_non_coded_test1_imputed, select = -Stone_type)
UK_ST_non_coded_train_outcome1_imputed <-
  subset(UK_ST_non_coded_train1_imputed, select = Stone_type)
UK_ST_non_coded_test_outcome1_imputed <-
  subset(UK_ST_non_coded_test1_imputed, select = Stone_type)

UK_ST_coded_train_predictors3_imputed <-
  UK_ST_coded_train_predictors1_imputed
UK_ST_coded_test_predictors3_imputed <-
  UK_ST_coded_test_predictors1_imputed

UK_ST_coded_train_predictors3_imputed$Sex <-
  as.numeric(UK_ST_coded_train_predictors3_imputed$Sex)
UK_ST_coded_test_predictors3_imputed$Sex <-
  as.numeric(UK_ST_coded_test_predictors3_imputed$Sex)
UK_ST_coded_train_predictors3_imputed$Age <-
  as.numeric(UK_ST_coded_train_predictors3_imputed$Age)
UK_ST_coded_test_predictors3_imputed$Age <-
  as.numeric(UK_ST_coded_test_predictors3_imputed$Age)
UK_ST_coded_train_predictors3_imputed$Urine_vol_L <-
  as.numeric(UK_ST_coded_train_predictors3_imputed$Urine_vol_L)
UK_ST_coded_test_predictors3_imputed$Urine_vol_L <-
  as.numeric(UK_ST_coded_test_predictors3_imputed$Urine_vol_L)
UK_ST_coded_train_predictors3_imputed$Urine_Ca_mmol_24h <-
  as.numeric(UK_ST_coded_train_predictors3_imputed$Urine_Ca_mmol_24h)
UK_ST_coded_test_predictors3_imputed$Urine_Ca_mmol_24h <-
  as.numeric(UK_ST_coded_test_predictors3_imputed$Urine_Ca_mmol_24h)
UK_ST_coded_train_predictors3_imputed$Urine_ox_mmol_24h <-
  as.numeric(UK_ST_coded_train_predictors3_imputed$Urine_ox_mmol_24h)
UK_ST_coded_test_predictors3_imputed$Urine_ox_mmol_24h <-
  as.numeric(UK_ST_coded_test_predictors3_imputed$Urine_ox_mmol_24h)
UK_ST_coded_train_predictors3_imputed$Urine_urate_mmol_24h <-
  as.numeric(UK_ST_coded_train_predictors3_imputed$Urine_urate_mmol_24h)
UK_ST_coded_test_predictors3_imputed$Urine_urate_mmol_24h <-
  as.numeric(UK_ST_coded_test_predictors3_imputed$Urine_urate_mmol_24h)
UK_ST_coded_train_predictors3_imputed$pH <-
  as.numeric(UK_ST_coded_train_predictors3_imputed$pH)
UK_ST_coded_test_predictors3_imputed$pH <-
  as.numeric(UK_ST_coded_test_predictors3_imputed$pH)
UK_ST_coded_train_predictors3_imputed$Hypercalciuria_coded <-
  as.integer(UK_ST_coded_train_predictors3_imputed$Hypercalciuria_coded)
UK_ST_coded_test_predictors3_imputed$Hypercalciuria_coded <-
  as.integer(UK_ST_coded_test_predictors3_imputed$Hypercalciuria_coded)
UK_ST_coded_train_predictors3_imputed$Hyperoxaluria_coded <-
  as.integer(UK_ST_coded_train_predictors3_imputed$Hyperoxaluria_coded)
UK_ST_coded_test_predictors3_imputed$Hyperoxaluria_coded <-
  as.integer(UK_ST_coded_test_predictors3_imputed$Hyperoxaluria_coded)
UK_ST_coded_train_predictors3_imputed$Hyperuricosuria_coded <-
  as.integer(UK_ST_coded_train_predictors3_imputed$Hyperuricosuria_coded)
UK_ST_coded_test_predictors3_imputed$Hyperuricosuria_coded <-
  as.integer(UK_ST_coded_test_predictors3_imputed$Hyperuricosuria_coded)

UK_ST_train_coded_predictors2_imputed <-
  as.matrix(UK_ST_coded_train_predictors3_imputed)
UK_ST_train_coded_outcome2_imputed <-
  as.matrix(UK_ST_coded_train_outcome1_imputed)
UK_ST_train_coded_outcome3_imputed <-
  to_categorical(UK_ST_train_coded_outcome2_imputed)
UK_ST_test_coded_predictors2_imputed <-
  as.matrix(UK_ST_coded_test_predictors3_imputed)
UK_ST_test_coded_outcome2_imputed <-
  as.matrix(UK_ST_coded_test_outcome1_imputed)
UK_ST_test_coded_outcome3_imputed <-
  to_categorical(as.factor(UK_ST_test_coded_outcome2_imputed))


## Recurrence
UK_Rec5_imputed <- UK[, -14:-17]
UK_Rec4_imputed <- UK[, -1:-13]
UK_Rec3_imputed <- UK_Rec4_imputed[, -1:-2]
UK_Rec2_imputed <- UK_Rec4_imputed[, -3:-4]
UK_Rec1_imputed <-
  cbind(UK_Rec5_imputed, UK_Rec3_imputed, UK_Rec2_imputed)

UK_Rec_coded_imputed1 <- subset(UK_Rec1_imputed,
                               select = -c(Stone_type,
                                           Recurrence)) %>% drop_na(Rec_coded)
UK_Rec_coded_imputed1$Rec_coded <-
  as.factor(UK_Rec_coded_imputed1$Rec_coded)
UK_Rec_coded_imputed2 <- mice(UK_Rec_coded_imputed1,
                        m = 4)
UK_Rec_coded_imputed <- complete(UK_Rec_coded_imputed2)
UK_Rec_coded_imputed <-
  UK_Rec_coded_imputed %>% mutate(Hyperuricosuria = ifelse(Urine_urate_mmol_24h >
                                                        4.46, "Yes", "No")) %>% na.omit()


UK_Rec_non_coded_imputed1 <- subset(UK_Rec1_imputed,
                                   select = -c(Rec_coded,
                                               Hyperuricosuria))
UK_Rec_non_coded_imputed1$Recurrence <-
  as.factor(UK_Rec_non_coded_imputed1$Recurrence)
UK_Rec_non_coded_imputed2 <- mice(UK_Rec_non_coded_imputed1,
                                  m = 4)
UK_Rec_non_coded_imputed <- complete(UK_Rec_non_coded_imputed2)
UK_Rec_non_coded_imputed <-
  UK_Rec_non_coded_imputed %>% mutate(Hyperuricosuria = ifelse(Urine_urate_mmol_24h >
                                                        4.46, "Yes", "No")) %>% na.omit()
UK_Rec_non_coded_imputed$Hyperuricosuria<-factor(UK_Rec_non_coded_imputed$Hyperuricosuria,
                                                 levels = c("Yes",
                                                            "No"))

UK_Rec_non_coded_sample_imputed <-
  sample(1:nrow(UK_Rec_non_coded_imputed),
         size = nrow(UK_Rec_non_coded_imputed) * 0.7)
UK_Rec_non_coded_train1_imputed <-
  UK_Rec_non_coded_imputed[UK_Rec_non_coded_sample_imputed, ]
UK_Rec_non_coded_test1_imputed <-
  UK_Rec_non_coded_imputed[-UK_Rec_non_coded_sample_imputed, ]
UK_Rec_non_coded_train_predictors1_imputed <-
  UK_Rec_non_coded_train1_imputed[, -16]
UK_Rec_non_coded_test_predictors1_imputed <-
  UK_Rec_non_coded_test1_imputed[, -16]
UK_Rec_non_coded_train_outcome1_imputed <-
  UK_Rec_non_coded_train1_imputed[, -1:-15]
UK_Rec_non_coded_test_outcome1_imputed <-
  UK_Rec_non_coded_test1_imputed[, -1:-15]

UK_Rec_coded_sample_imputed <-
  sample(1:nrow(UK_Rec_coded_imputed),
         size = nrow(UK_Rec_coded_imputed) * 0.7)
UK_Rec_coded_train1_imputed <-
  UK_Rec_coded_imputed[UK_Rec_coded_sample_imputed, ]
UK_Rec_coded_test1_imputed <-
  UK_Rec_coded_imputed[-UK_Rec_coded_sample_imputed, ]
UK_Rec_coded_train_predictors1_imputed <-
  subset(UK_Rec_coded_train1_imputed,
select = -Rec_coded)
UK_Rec_coded_test_predictors1_imputed <-
  subset(UK_Rec_coded_test1_imputed,
select = -Rec_coded)
UK_Rec_coded_train_outcome1_imputed <-
  subset(UK_Rec_coded_train1_imputed,
         select = Rec_coded)
UK_Rec_coded_test_outcome1_imputed <-
  subset(UK_Rec_coded_test1_imputed,
         select = Rec_coded)

UK_Rec_coded_train_predictors3_imputed <-
  UK_Rec_coded_train_predictors1_imputed[, -11:-14]
UK_Rec_coded_test_predictors3_imputed <-
  UK_Rec_coded_test_predictors1_imputed[, -11:-14]
UK_Rec_coded_train_predictors3_imputed$Sex <-
  as.integer(UK_Rec_coded_train_predictors3_imputed$Sex)
UK_Rec_coded_test_predictors3_imputed$Sex <-
  as.integer(UK_Rec_coded_test_predictors3_imputed$Sex)

UK_Rec_train_coded_predictors2_imputed <-
  as.matrix(UK_Rec_coded_train_predictors3_imputed)
UK_Rec_train_coded_outcome2_imputed <-
  as.numeric(UK_Rec_coded_train_outcome1_imputed$Rec_coded)
UK_Rec_test_coded_predictors2_imputed <-
  as.matrix(UK_Rec_coded_test_predictors3_imputed)
UK_Rec_test_coded_outcome2_imputed <-
  as.numeric(UK_Rec_coded_test_outcome1_imputed$Rec_coded)

# Oversampled
## Stone Type
UK_ST1_coded <- UK_ST[, -14] %>% drop_na(Stone.Coded)
UK_ST1_coded$Stone.Coded <- as.factor(UK_ST1_coded$Stone.Coded)
UK_ST1_coded_oversample <- caret::upSample(x = UK_ST1_coded[, -14],
                                           y = UK_ST1_coded$Stone.Coded)
UK_ST1_coded_oversample <- na.omit(UK_ST1_coded_oversample)

UK_ST1_non_coded <- UK_ST[, -15] %>% drop_na(Stone_type)
UK_ST1_non_coded$Stone_type <- factor(UK_ST1_non_coded$Stone_type,
                                      levels = c("CaOx",
                                                 "CaP",
                                                 "Unknown",
                                                 "Urate"))

UK_ST1_non_coded_oversample <-
  caret::upSample(x = UK_ST1_non_coded[, -14],
                  y = UK_ST1_non_coded$Stone_type)
UK_ST1_non_coded_oversample <- na.omit(UK_ST1_non_coded_oversample)

UK_ST1_coded_sample_oversample <-
  sample(1:nrow(UK_ST1_coded_oversample),
         size = nrow(UK_ST1_coded_oversample) * 0.7)
UK_ST_coded_train1_oversample <-
  UK_ST1_coded_oversample[UK_ST1_coded_sample_oversample, ]
UK_ST_coded_test1_oversample <-
  UK_ST1_coded_oversample[-UK_ST1_coded_sample_oversample, ]
UK_ST_coded_train_predictors1_oversample <-
  UK_ST_coded_train1_oversample[, -14]
UK_ST_coded_test_predictors1_oversample <-
  UK_ST_coded_test1_oversample[, -14]
UK_ST_coded_train_outcome1_oversample <-
  UK_ST_coded_train1_oversample[, -1:-13]
UK_ST_coded_test_outcome1_oversample <-
  UK_ST_coded_test1_oversample[, -1:-13]

UK_ST1_non_coded_sample_oversample <-
  sample(1:nrow(UK_ST1_non_coded_oversample),
         size = nrow(UK_ST1_non_coded_oversample) * 0.7)
UK_ST_non_coded_train1_oversample <-
  UK_ST1_non_coded_oversample[UK_ST1_non_coded_sample_oversample, ]
UK_ST_non_coded_test1_oversample <-
  UK_ST1_non_coded[-UK_ST1_non_coded_sample_oversample, ]
UK_ST_non_coded_train_predictors1_oversample <-
  UK_ST_non_coded_train1_oversample[, -14]
UK_ST_non_coded_test_predictors1_oversample <-
  UK_ST_non_coded_test1_oversample[, -14]
UK_ST_non_coded_train_outcome1_oversample <-
  UK_ST_non_coded_train1_oversample[, -1:-13]
UK_ST_non_coded_test_outcome1_oversample <-
  UK_ST_non_coded_test1_oversample[, -1:-13]

UK_ST_coded_train_predictors3_oversample <-
  UK_ST_coded_train_predictors1_oversample[, -11:-13]
UK_ST_coded_test_predictors3_oversample <-
  UK_ST_coded_test_predictors1_oversample[, -11:-13]

UK_ST_coded_train_predictors3_oversample$Sex <-
  as.numeric(UK_ST_coded_train_predictors3_oversample$Sex)
UK_ST_coded_test_predictors3_oversample$Sex <-
  as.numeric(UK_ST_coded_test_predictors3_oversample$Sex)
UK_ST_coded_train_predictors3_oversample$Age <-
  as.numeric(UK_ST_coded_train_predictors3_oversample$Age)
UK_ST_coded_test_predictors3_oversample$Age <-
  as.numeric(UK_ST_coded_test_predictors3_oversample$Age)
UK_ST_coded_train_predictors3_oversample$Urine_vol_L <-
  as.numeric(UK_ST_coded_train_predictors3_oversample$Urine_vol_L)
UK_ST_coded_test_predictors3_oversample$Urine_vol_L <-
  as.numeric(UK_ST_coded_test_predictors3_oversample$Urine_vol_L)
UK_ST_coded_train_predictors3_oversample$Urine_Ca_mmol_24h <-
  as.numeric(UK_ST_coded_train_predictors3_oversample$Urine_Ca_mmol_24h)
UK_ST_coded_test_predictors3_oversample$Urine_Ca_mmol_24h <-
  as.numeric(UK_ST_coded_test_predictors3_oversample$Urine_Ca_mmol_24h)
UK_ST_coded_train_predictors3_oversample$Urine_ox_mmol_24h <-
  as.numeric(UK_ST_coded_train_predictors3_oversample$Urine_ox_mmol_24h)
UK_ST_coded_test_predictors3_oversample$Urine_ox_mmol_24h <-
  as.numeric(UK_ST_coded_test_predictors3_oversample$Urine_ox_mmol_24h)
UK_ST_coded_train_predictors3_oversample$Urine_urate_mmol_24h <-
  as.numeric(UK_ST_coded_train_predictors3_oversample$Urine_urate_mmol_24h)
UK_ST_coded_test_predictors3_oversample$Urine_urate_mmol_24h <-
  as.numeric(UK_ST_coded_test_predictors3_oversample$Urine_urate_mmol_24h)
UK_ST_coded_train_predictors3_oversample$pH <-
  as.numeric(UK_ST_coded_train_predictors3_oversample$pH)
UK_ST_coded_test_predictors3_oversample$pH <-
  as.numeric(UK_ST_coded_test_predictors3_oversample$pH)
UK_ST_coded_train_predictors3_oversample$Hypercalciuria_coded <-
  as.integer(UK_ST_coded_train_predictors3_oversample$Hypercalciuria_coded)
UK_ST_coded_test_predictors3_oversample$Hypercalciuria_coded <-
  as.integer(UK_ST_coded_test_predictors3_oversample$Hypercalciuria_coded)
UK_ST_coded_train_predictors3_oversample$Hyperoxaluria_coded <-
  as.integer(UK_ST_coded_train_predictors3_oversample$Hyperoxaluria_coded)
UK_ST_coded_test_predictors3_oversample$Hyperoxaluria_coded <-
  as.integer(UK_ST_coded_test_predictors3_oversample$Hyperoxaluria_coded)
UK_ST_coded_train_predictors3_oversample$Hyperuricosuria_coded <-
  as.integer(UK_ST_coded_train_predictors3_oversample$Hyperuricosuria_coded)
UK_ST_coded_test_predictors3_oversample$Hyperuricosuria_coded <-
  as.integer(UK_ST_coded_test_predictors3_oversample$Hyperuricosuria_coded)

UK_ST_train_coded_predictors2_oversample <-
  as.matrix(UK_ST_coded_train_predictors3_oversample)
UK_ST_train_coded_outcome2_oversample <-
  as.matrix(UK_ST_coded_train_outcome1_oversample)
UK_ST_train_coded_outcome3_oversample <-
  to_categorical(UK_ST_train_coded_outcome2_oversample)
UK_ST_test_coded_predictors2_oversample <-
  as.matrix(UK_ST_coded_test_predictors3_oversample)
UK_ST_test_coded_outcome2_oversample <-
  as.matrix(UK_ST_coded_test_outcome1_oversample)
UK_ST_test_coded_outcome3_oversample <-
  to_categorical(UK_ST_test_coded_outcome2_oversample)


## Recurrence
UK_Rec_oversample <- UK
UK_Rec5_oversample <- UK_Rec_oversample[, -14:-17]
UK_Rec4_oversample <- UK_Rec_oversample[, -1:-13]
UK_Rec3_oversample <- UK_Rec4_oversample[, -1:-2]
UK_Rec2_oversample <- UK_Rec4_oversample[, -3:-4]
UK_Rec1_oversample <-
  cbind(UK_Rec5_oversample, UK_Rec3_oversample, UK_Rec2_oversample)



UK_Rec_coded_oversample <- UK_Rec1_oversample[, -16]
UK_Rec_coded_oversample$Rec_coded <-
  as.factor(UK_Rec_coded_oversample$Rec_coded)
UK_Rec_coded_oversample <-
  UK_Rec_coded_oversample %>% drop_na(Rec_coded)
UK_Rec_coded_oversample1 <- ROSE::ovun.sample(Rec_coded ~ .,
                                             UK_Rec_coded_oversample,
                                             method = "over",
                                             seed = 1234)
UK_Rec_coded_oversample<-UK_Rec_coded_oversample1$data %>% as_tibble() %>% na.omit()

UK_Rec_non_coded_oversample <- UK_Rec1_oversample[, -17]
UK_Rec_non_coded_oversample$Recurrence <- 
  as.factor(UK_Rec_non_coded_oversample$Recurrence)
UK_Rec_non_coded_oversample <- UK_Rec_non_coded_oversample %>% drop_na(Recurrence)
UK_Rec_non_coded_oversample1 <- ROSE::ovun.sample(Recurrence ~.,
                                                 UK_Rec_non_coded_oversample,
                                                 method = "over",
                                                 seed = 1234)
UK_Rec_non_coded_oversample <- UK_Rec_non_coded_oversample1$data %>% as_tibble() %>% na.omit()

UK_Rec_non_coded_sample_oversample <-
  sample(1:nrow(UK_Rec_non_coded_oversample),
         size = (nrow(UK_Rec_non_coded_oversample) * 0.7))
UK_Rec_non_coded_train1_oversample <-
  UK_Rec_non_coded_oversample[UK_Rec_non_coded_sample_oversample,]
UK_Rec_non_coded_test1_oversample <-
  UK_Rec_non_coded_oversample[-UK_Rec_non_coded_sample_oversample,]
UK_Rec_non_coded_train_predictors1_oversample <-
  UK_Rec_non_coded_train1_oversample[,-16]
UK_Rec_non_coded_test_predictors1_oversample <-
  UK_Rec_non_coded_test1_oversample[,-16]
UK_Rec_non_coded_train_outcome1_oversample <-
  UK_Rec_non_coded_train1_oversample[,-1:-15]
UK_Rec_non_coded_test_outcome1_oversample <-
  UK_Rec_non_coded_test1_oversample[,-1:-15]

UK_Rec_coded_sample_oversample <-
  sample(1:nrow(UK_Rec_coded_oversample),
         size = (nrow(UK_Rec_coded_oversample) * 0.7))
UK_Rec_coded_train1_oversample <-
  UK_Rec_coded_oversample[UK_Rec_coded_sample_oversample,]
UK_Rec_coded_test1_oversample <-
  UK_Rec_coded_oversample[-UK_Rec_coded_sample_oversample,]
UK_Rec_coded_train_predictors1_oversample <-
  UK_Rec_coded_train1_oversample[,-16]
UK_Rec_coded_test_predictors1_oversample <-
  UK_Rec_coded_test1_oversample[,-16]
UK_Rec_coded_train_outcome1_oversample <-
  UK_Rec_coded_train1_oversample[,-1:-15]
UK_Rec_coded_test_outcome1_oversample <-
  UK_Rec_coded_test1_oversample[,-1:-15]

UK_Rec_coded_train_predictors3_oversample <-
  UK_Rec_coded_train_predictors1_oversample[,-11:-14]
UK_Rec_coded_test_predictors3_oversample <-
  UK_Rec_coded_test_predictors1_oversample[,-11:-14]
UK_Rec_coded_train_predictors3_oversample$Sex <-
  as.integer(UK_Rec_coded_train_predictors3_oversample$Sex)
UK_Rec_coded_test_predictors3_oversample$Sex <-
  as.integer(UK_Rec_coded_test_predictors3_oversample$Sex)

UK_Rec_train_coded_predictors2_oversample <-
  as.matrix(UK_Rec_coded_train_predictors3_oversample)
UK_Rec_train_coded_outcome2_oversample <-
  as.numeric(UK_Rec_coded_train_outcome1_oversample$Rec_coded)
UK_Rec_test_coded_predictors2_oversample <-
  as.matrix(UK_Rec_coded_test_predictors3_oversample)
UK_Rec_test_coded_outcome2_oversample <-
  as.numeric(UK_Rec_coded_test_outcome1_oversample$Rec_coded)

UK_Rec_train_coded_predictors2_oversample <-
  as.matrix(UK_Rec_coded_train_predictors3_oversample)
UK_Rec_train_coded_outcome2_oversample <-
  as.numeric(UK_Rec_coded_train_outcome1_oversample$Rec_coded)
UK_Rec_test_coded_predictors2_oversample <-
  as.matrix(UK_Rec_coded_test_predictors3_oversample)
UK_Rec_test_coded_outcome2_oversample <-
  as.numeric(UK_Rec_coded_test_outcome1_oversample$Rec_coded)


UK_non_coded_test_caox <-
  ifelse(UK_ST_non_coded_test1$Stone_type == "CaOx", 1, 0) %>% factor(levels = c("0",
                                                                                 "1"))
UK_non_coded_test_cap <-
  ifelse(UK_ST_non_coded_test1$Stone_type == "CaP", 1, 0) %>% factor(levels = c("0",
                                                                                "1"))
UK_non_coded_test_urate <-
  ifelse(UK_ST_non_coded_test1$Stone_type == "Urate", 1, 0) %>% factor(levels = c("0",
                                                                                  "1"))

Swiss_non_coded_test_caox <-
  ifelse(Swiss_ST_non_coded_test1$Stone_type == "CaOx", 1, 0) %>% factor(levels = c("0",
                                                                                    "1"))
Swiss_non_coded_test_cap <-
  ifelse(Swiss_ST_non_coded_test1$Stone_type == "CaP", 1, 0) %>% factor(levels = c("0",
                                                                                   "1"))
Swiss_non_coded_test_urate <-
  ifelse(Swiss_ST_non_coded_test1$Stone_type == "Urate", 1, 0) %>% factor(levels = c("0",
                                                                                     "1"))


UK_ST_non_coded_test1$Stone_type <-
  factor(UK_ST_non_coded_test1$Stone_type,
         levels = c("CaOx",
                    "CaP",
                    "Unknown",
                    "Urate"))
Swiss_ST_non_coded_test1$Stone_type <-
  factor(Swiss_ST_non_coded_test1$Stone_type,
         levels = c("CaOx",
                    "CaP",
                    "Unknown",
                    "Urate"))

Swiss_ST_non_coded_train1$Stone_type <-
  factor(Swiss_ST_non_coded_train1$Stone_type,
         levels = c("CaOx",
                    "CaP",
                    "Unknown",
                    "Urate"))
Swiss_ST_all_non_coded <- rbind(Swiss_ST_non_coded_train1,
                                Swiss_ST_non_coded_test1)

Swiss_ST_all_non_coded_caox <-
  ifelse(Swiss_ST_all_non_coded$Stone_type == "CaOx", 1, 0) %>% factor(levels = c("0",
                                                                                  "1"))
Swiss_ST_all_non_coded_cap <-
  ifelse(Swiss_ST_all_non_coded$Stone_type == "CaP", 1, 0) %>% factor(levels = c("0",
                                                                                 "1"))
Swiss_ST_all_non_coded_urate <-
  ifelse(Swiss_ST_all_non_coded$Stone_type == "Urate", 1, 0) %>% factor(levels = c("0",
                                                                                   "1"))

Swiss_ST_test_coded_outcome3 <-
  Swiss_ST_test_coded_outcome3[, -1] %>% as_tibble()
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