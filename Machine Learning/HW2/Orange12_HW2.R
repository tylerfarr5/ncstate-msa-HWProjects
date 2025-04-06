########################################################################
############# Reading in the data ####################################
######################################################################
library(ggplot2)
library(tidyverse)
library(earth)
library(ROCR)
library(ROCit)
library(mgcv)
library(randomForest)
library(xgboost)
library(pROC)
library(caret)

insurance_t <- read.csv("insurance_t.csv")

#############################################################################
############# Exploring the data ############################################
##############################################################################
#exploring
View(insurance_t)
str(insurance_t)
summary(insurance_t)

hist(insurance_t$DDABAL)
hist(insurance_t$HMVAL)
hist(insurance_t$CHECKS)
hist(insurance_t$ACCTAGE)

#replaces missing vals with median for numeric columns
insurance_t <- insurance_t %>% 
  mutate(across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE))))

#convert to factor
insurance_t$DDA <- as.factor(insurance_t$DDA)
insurance_t$DIRDEP <- as.factor(insurance_t$DIRDEP)
insurance_t$NSF <- as.factor(insurance_t$NSF)
insurance_t$SAV <- as.factor(insurance_t$SAV)
insurance_t$ATM <- as.factor(insurance_t$ATM)
insurance_t$CD <- as.factor(insurance_t$CD)
insurance_t$IRA <- as.factor(insurance_t$IRA)
insurance_t$INV <- as.factor(insurance_t$INV)
insurance_t$MM <- as.factor(insurance_t$MM)
insurance_t$MMCRED <- as.factor(insurance_t$MMCRED)
insurance_t$CC <- as.factor(insurance_t$CC)
insurance_t$CCPURC <- as.factor(insurance_t$CCPURC)
insurance_t$SDB <- as.factor(insurance_t$SDB)
insurance_t$INAREA <- as.factor(insurance_t$INAREA)
insurance_t$BRANCH <- as.factor(insurance_t$BRANCH)
insurance_t$INS <- as.factor(insurance_t$INS)

########################################################################
####################### Question 1 - Random Forest approach########################
########################################################################

############################################ creating the model
set.seed(123)
rf.ins <- randomForest(INS ~ ., data = insurance_t, ntree = 500, importance = TRUE)

#plotting to see if we can shorten ntrees
plot(rf.ins, main = "Number of Trees Compared to MSE")


######################################## tuning the model

set.seed(123)
tuneRF(x = insurance_t[,-37], y = insurance_t[,37], 
       plot = TRUE, ntreeTry = 500, stepFactor = 0.5)

#above output said optimal mtry is 6. rerun random forest with optimal values (reduces comp time)
set.seed(123)
rf.ins <- randomForest(INS ~ ., data = insurance_t, ntree = 500, mtry = 6, importance = TRUE)


################################################# variable selection
insurance_t$random <- rnorm(8495) #length of training set

set.seed(123)
rf.ins <- randomForest(INS ~ ., data = insurance_t, ntree = 500, mtry = 6, importance = TRUE)

############################################# var imp plot with rand
varImpPlot(rf.ins,
           sort = TRUE,
           n.var = 37,
           main = "Look for Variables Below Random Variable")

#AGE, NSF, CRSCORE, INAREA, LORES
#both plots highlight NSF, INAREA
############################################### var imp plot w/o rand
set.seed(123)
rf.ins <- randomForest(INS ~ .- random - AGE - NSF - CRSCORE - INAREA - LORES, data = insurance_t, ntree = 500, mtry = 6, importance = TRUE)

varImpPlot(rf.ins,
           sort = TRUE,
           n.var = 12,
           main = "Order of Variables")

importance(rf.ins, type = 1)


############################################ ROC Curve

p_hat <- predict(rf.ins, type = "prob")
logit_roc <- rocit(p_hat[,2],insurance_t$INS)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)


########################################################################
####################### Question 2 - XGBoost approach ########################
########################################################################

############################################ creating the model
train_x <- model.matrix(INS ~ .-random , data = insurance_t)[, -1]
train_y <- as.numeric(insurance_t$INS)-1

set.seed(123)
xgb.ins <- xgboost(data = train_x, label = train_y, subsample = 0.5, nrounds = 13, eta = 0.27, max_depth = 6, objective = "binary:logistic")


######### CV to minimize number of trees initially at 11! then 13
set.seed(123)
xgbcv.ins <- xgb.cv(data = train_x, label = train_y, subsample = 0.5, nrounds = 50, nfold = 10) #, max_depth = 6, eta = 0.27)


############# grid search to optimize parameters
tune_grid <- expand.grid(
  nrounds = 13,
  eta = c(0.2, 0.23, 0.25, 0.27, 0.3),
  max_depth = c(1:8),
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = c(0, 0.25, 0.5, 0.75,1)
)

set.seed(123) #
xgb.ins.caret <- train(x = train_x, y = train_y,
                        method = "xgbTree",
                        tuneGrid = tune_grid,
                        trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                 number = 10),
                       objective = "binary:logistic") #

plot(xgb.ins.caret)

xgb.ins.caret$bestTune


#################################### AUROC comparison

p_hat <- predict(xgb.ins, newdata = train_x, type = "response")
logit_roc <- rocit(p_hat, insurance_t$INS)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)


################################### variable importance

#using optimal parameters from previous tuning
xgb.importance(feature_names = colnames(train_x), model = xgb.ins)

#will automatically cluster variables statistically based on similar gain
xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb.ins))
