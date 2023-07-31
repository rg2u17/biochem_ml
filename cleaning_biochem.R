library(keras)
library(tensorflow)
library(lime)
library(dplyr)
library(caret)
library(neuralnet)
library(xgboost)
library(lightgbm)
library(rpart)
library(randomForest)
library(MLeval)
library(caTools)

#import and sort British data
UK <-
  read.csv("~/Desktop/Sayer/Nomogram/R/R directory/Biochemistry ML/Combined_Data2.csv")
Swiss <-
  read.csv("~/Desktop/Sayer/Nomogram/R/R directory/Biochemistry ML/Swiss.csv")

#Recurrence (includes Stone Type)
UK$Urine_vol_L <- round(UK$Urine_vol_L, 3)
UK$Urine_Ca_mmol_24h <- round(UK$Urine_vol_L, 3)
UK$Urine_ox_mmol_24h <- round(UK$Urine_ox_mmol_24h, 3)
UK$Urine_urate_mmol_24h <- round(UK$Urine_urate_mmol_24h, 3)
UK$pH <- round(UK$pH, 3)

UK_Rec <- na.omit(UK)
UK_Rec$Sex <- as.factor(UK_Rec$Sex)
UK_Rec$Hypercalciuria_coded <- as.integer(UK_Rec$Hypercalciuria_coded)
UK_Rec$Hypercalciuria <- as.factor(UK_Rec$Hypercalciuria)
UK_Rec$Hyperoxaluria_coded <- as.integer(UK_Rec$Hyperoxaluria_coded)
UK_Rec$Hyperoxaluria <- as.factor(UK_Rec$Hyperoxaluria)
UK_Rec$Hyperuricosuria_coded <-
  as.integer(UK_Rec$Hyperuricosuria_coded)
UK_Rec$Hyperuricosuria <- as.factor(UK_Rec$Hyperuricosuria)
UK_Rec$Recurrence <- factor(UK_Rec$Recurrence,
                            levels = c("Yes",
                                       "No"))
UK_Rec$Rec_coded <- as.integer(UK_Rec$Rec_coded)
UK_Rec$Stone_type <- as.factor(UK_Rec$Stone_type)
UK_Rec$Stone.Coded <- as.integer(UK_Rec$Stone.Coded)

summary(UK_Rec)
str(UK_Rec)

Swiss1 <- Swiss
Swiss1$Hypercalciuria_coded <- as.factor(Swiss1$Hypercalciuria_coded)
Swiss1$Hyperoxaluria_coded <- as.factor(Swiss1$Hyperoxaluria_coded)
Swiss1$Hyperuricosuria_coded <-
  as.factor(Swiss1$Hyperuricosuria_coded)
Swiss1$Hypercalciuria <-
  ifelse(Swiss1$Hypercalciuria_coded == "2", "Yes", "No")
Swiss1$Hyperoxaluria <-
  ifelse(Swiss1$Hyperoxaluria_coded == "2", "Yes", "No")
Swiss1$Hyperuricosuria <-
  ifelse(Swiss1$Hyperuricosuria_coded == "2", "Yes", "No")

Swiss1$Urine_vol_L <- as.numeric(Swiss1$Urine_vol_L)
Swiss1$Urine_Ca_mmol_24h <- as.numeric(Swiss1$Urine_vol_L)
Swiss1$Urine_ox_mmol_24h <- as.numeric(Swiss1$Urine_ox_mmol_24h)
Swiss1$Urine_urate_mmol_24h <- as.numeric(Swiss1$Urine_urate_mmol_24h)
Swiss1$pH <- as.numeric(Swiss1$pH)


Swiss1$Urine_vol_L <- round(Swiss1$Urine_vol_L, 3)
Swiss1$Urine_Ca_mmol_24h <- round(Swiss1$Urine_vol_L, 3)
Swiss1$Urine_ox_mmol_24h <- round(Swiss1$Urine_ox_mmol_24h, 3)
Swiss1$Urine_urate_mmol_24h <- round(Swiss1$Urine_urate_mmol_24h, 3)
Swiss1$pH <- round(Swiss1$pH, 3)

Swiss_coded2 <- Swiss1[, -11:-17]
Swiss_coded3 <- Swiss1[, -15:-17]
Swiss_coded4 <- Swiss_coded3[, -1:-10]
Swiss_hypers <- Swiss1[, -1:-14]
Swiss_overall <- cbind(Swiss_coded2, Swiss_hypers, Swiss_coded4)

Swiss_Rec <- na.omit(Swiss_overall)
Swiss_ST <- na.omit(Swiss_overall[, -14:-15])

Swiss_Rec$Sex <- as.factor(Swiss_Rec$Sex)
Swiss_Rec$Hypercalciuria_coded <-
  as.integer(Swiss_Rec$Hypercalciuria_coded)
Swiss_Rec$Hypercalciuria <- as.factor(Swiss_Rec$Hypercalciuria)
Swiss_Rec$Hyperoxaluria_coded <-
  as.integer(Swiss_Rec$Hyperoxaluria_coded)
Swiss_Rec$Hyperoxaluria <- as.factor(Swiss_Rec$Hyperoxaluria)
Swiss_Rec$Hyperuricosuria_coded <-
  as.integer(Swiss_Rec$Hyperuricosuria_coded)
Swiss_Rec$Hyperuricosuria <- as.factor(Swiss_Rec$Hyperuricosuria)
Swiss_Rec$Recurrence <- as.factor(Swiss_Rec$Recurrence)
Swiss_Rec$Rec_coded <- as.integer(Swiss_Rec$Rec_coded)
Swiss_Rec$Stone_type <- as.factor(Swiss_Rec$Stone_type)
Swiss_Rec$Stone.Coded <- as.integer(Swiss_Rec$Stone.Coded)

#Stone Type (Excludes Recurrence)
UK_ST <- UK[, -14:-15]
UK_ST1 <- na.omit(UK_ST)
UK_ST1$Sex <- as.factor(UK_ST1$Sex)
UK_ST1$Hypercalciuria_coded <- as.integer(UK_ST1$Hypercalciuria_coded)
UK_ST1$Hypercalciuria <- as.factor(UK_ST1$Hypercalciuria)
UK_ST1$Hyperoxaluria_coded <- as.integer(UK_ST1$Hyperoxaluria_coded)
UK_ST1$Hyperoxaluria <- as.factor(UK_ST1$Hyperoxaluria)
UK_ST1$Hyperuricosuria_coded <-
  as.integer(UK_ST1$Hyperuricosuria_coded)
UK_ST1$Hyperuricosuria <- as.factor(UK_ST1$Hyperuricosuria)
UK_ST1$Stone_type <- as.factor(UK_ST1$Stone_type)
UK_ST1$Stone.Coded <- as.integer(UK_ST1$Stone.Coded)

summary(UK_ST1)
str(UK_ST1)

Swiss_ST$Sex <- as.factor(Swiss_ST$Sex)
Swiss_ST$Hypercalciuria_coded <-
  as.integer(Swiss_ST$Hypercalciuria_coded)
Swiss_ST$Hypercalciuria <- as.factor(Swiss_ST$Hypercalciuria)
Swiss_ST$Hyperoxaluria_coded <-
  as.integer(Swiss_ST$Hyperoxaluria_coded)
Swiss_ST$Hyperoxaluria <- as.factor(Swiss_ST$Hyperoxaluria)
Swiss_ST$Hyperuricosuria_coded <-
  as.integer(Swiss_ST$Hyperuricosuria_coded)
Swiss_ST$Hyperuricosuria <- as.factor(Swiss_ST$Hyperuricosuria)
Swiss_ST$Stone_type <- as.factor(Swiss_ST$Stone_type)
Swiss_ST$Stone.Coded <- as.integer(Swiss_ST$Stone.Coded)