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
library(nnet)
library(patchwork)
library(NeuralNetTools)
library(pdp)
library(iml)

insurance_t <- read.csv("insurance_t.csv")
insurance_v <- read.csv("insurance_v.csv")

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

insurance_v <- insurance_v %>% 
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

insurance_v$DDA <- as.factor(insurance_v$DDA)
insurance_v$DIRDEP <- as.factor(insurance_v$DIRDEP)
insurance_v$NSF <- as.factor(insurance_v$NSF)
insurance_v$SAV <- as.factor(insurance_v$SAV)
insurance_v$ATM <- as.factor(insurance_v$ATM)
insurance_v$CD <- as.factor(insurance_v$CD)
insurance_v$IRA <- as.factor(insurance_v$IRA)
insurance_v$INV <- as.factor(insurance_v$INV)
insurance_v$MM <- as.factor(insurance_v$MM)
insurance_v$MMCRED <- as.factor(insurance_v$MMCRED)
insurance_v$CC <- as.factor(insurance_v$CC)
insurance_v$CCPURC <- as.factor(insurance_v$CCPURC)
insurance_v$SDB <- as.factor(insurance_v$SDB)
insurance_v$INAREA <- as.factor(insurance_v$INAREA)
insurance_v$BRANCH <- as.factor(insurance_v$BRANCH)
insurance_v$INS <- as.factor(insurance_v$INS)


#######################################################################
############### Neural Network #########################################
##############################################################

#scaling the predictors (they are skewed so min/max standardization)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


insurance_t <- insurance_t %>%
  mutate(s_ACCTAGE = normalize(ACCTAGE),
         s_DDABAL = normalize(DDABAL),
         s_DEP = normalize(DEP) ,
         s_DEPAMT = normalize(DEPAMT), 
         s_CHECKS = normalize(CHECKS),
         s_NSFAMT = normalize(NSFAMT),
         s_PHONE = normalize(PHONE),
         s_TELLER = normalize(TELLER),
         s_SAVBAL = normalize(SAVBAL),
         s_ATMAMT = normalize(ATMAMT),
         s_POS = normalize(POS),
         s_POSAMT = normalize(POSAMT),
         s_CDBAL = normalize(CDBAL),
         s_IRABAL = normalize(IRABAL),
         s_INVBAL = normalize(INVBAL),
         s_MMBAL = normalize(MMBAL),
         s_CCBAL = normalize(CCBAL),
         s_INCOME = normalize(INCOME),
         s_LORES = normalize(LORES),
         s_HMVAL = normalize(HMVAL),
         s_AGE = normalize(AGE),
         s_CRSCORE = normalize(CRSCORE))

insurance_v <- insurance_v %>%
  mutate(s_ACCTAGE = normalize(ACCTAGE),
         s_DDABAL = normalize(DDABAL),
         s_DEP = normalize(DEP) ,
         s_DEPAMT = normalize(DEPAMT), 
         s_CHECKS = normalize(CHECKS),
         s_NSFAMT = normalize(NSFAMT),
         s_PHONE = normalize(PHONE),
         s_TELLER = normalize(TELLER),
         s_SAVBAL = normalize(SAVBAL),
         s_ATMAMT = normalize(ATMAMT),
         s_POS = normalize(POS),
         s_POSAMT = normalize(POSAMT),
         s_CDBAL = normalize(CDBAL),
         s_IRABAL = normalize(IRABAL),
         s_INVBAL = normalize(INVBAL),
         s_MMBAL = normalize(MMBAL),
         s_CCBAL = normalize(CCBAL),
         s_INCOME = normalize(INCOME),
         s_LORES = normalize(LORES),
         s_HMVAL = normalize(HMVAL),
         s_AGE = normalize(AGE),
         s_CRSCORE = normalize(CRSCORE))

##############################################
#####
###############################################
nnet_train <- insurance_t %>%
  select(-c(ACCTAGE, DDABAL, DEP, DEPAMT, CHECKS,  NSFAMT, PHONE,  TELLER , SAVBAL , ATMAMT ,POS ,POSAMT , CDBAL , IRABAL ,INVBAL , MMBAL ,CCBAL , INCOME , LORES , HMVAL ,AGE , CRSCORE))

#only builds single hidden layer NN
set.seed(12345)
nn.bank <- nnet(INS ~ ., data = nnet_train, size = 7, decay =1, linout = FALSE) #size is # of neurons (tune)
#linout = TRUE is continuous response, FALSE = categorical response

plotnet(nn.bank)

#Optimize number of hidden nodes and decay


tune_grid <- expand.grid(
  .size = c(2, 3, 4, 5, 6, 7), #more neurons = more complexity
  .decay = c(0, 0.5, 1) #penalty parameter on coefficients - prevents complexity/overfitting
)

set.seed(12345)
nn.bank.caret <- caret::train(INS ~ .
                              , data = nnet_train,
                              method = "nnet", 
                              tuneGrid = tune_grid,
                              trControl = trainControl(method = 'cv', number = 10),
                              trace = FALSE, linout = FALSE)

nn.bank.caret$bestTune #says size 7, decay 1


######################################################################
################ XGBoost ############################################
####################################################################

####################### Question 2 - XGBoost approach ########################
########################################################################

############################################ creating the model
train_x <- model.matrix(INS ~ . -s_ACCTAGE - s_DDABAL -s_DEP - s_DEPAMT - s_CHECKS - s_NSFAMT - s_PHONE - s_TELLER - s_SAVBAL - s_ATMAMT -s_POS - s_POSAMT - s_CDBAL - s_IRABAL -s_INVBAL - s_MMBAL -s_CCBAL - s_INCOME - s_LORES - s_HMVAL -s_AGE - s_CRSCORE
                        - MMCRED, data = insurance_t)[, -1]
train_y <- as.numeric(insurance_t$INS)-1

set.seed(12345)
xgb.ins <- xgboost(data = train_x, label = train_y, subsample = 0.5, nrounds = 13, eta = 0.27, max_depth = 6, objective = "binary:logistic")

tune_grid <- expand.grid(
  nrounds = 13,
  eta = 0.27,
  max_depth = 6,
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 0.5
)

set.seed(12345) #
xgb.ins.caret <- train(x = train_x, y = train_y,
                       method = "xgbTree",
                       tuneGrid = tune_grid,
                       objective = "binary:logistic") #



p_hat <- predict(xgb.ins, newdata = train_x, type = "response")
logit_roc <- rocit(p_hat, insurance_t$INS)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)


######################################################################
########## Testing XGBoost on validation data ##############################
#####################################################################

valid_x <- model.matrix(INS ~ . -s_ACCTAGE - s_DDABAL -s_DEP - s_DEPAMT - s_CHECKS - s_NSFAMT - s_PHONE - s_TELLER - s_SAVBAL - s_ATMAMT -s_POS - s_POSAMT - s_CDBAL - s_IRABAL -s_INVBAL - s_MMBAL -s_CCBAL - s_INCOME - s_LORES - s_HMVAL -s_AGE - s_CRSCORE
                         -MMCRED , data = insurance_v)[, -1]
valid_y <- as.numeric(insurance_v$INS)-1

p_hat <- predict(xgb.ins, newdata = valid_x, type = "response")
logit_roc <- rocit(p_hat, insurance_v$INS)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)


#####################################################################
################# Testing Neural Network on Validation Data ###########
######################################################################
nnet_valid <- insurance_v %>%
  select(-c(ACCTAGE, DDABAL, DEP, DEPAMT, CHECKS,  NSFAMT, PHONE,  TELLER , SAVBAL , ATMAMT ,POS ,POSAMT , CDBAL , IRABAL ,INVBAL , MMBAL ,CCBAL , INCOME , LORES , HMVAL ,AGE , CRSCORE))

p_hat <- predict(nn.bank, newdata = nnet_valid, type = "class")
logit_roc <- rocit(as.numeric(p_hat), insurance_v$INS)
plot(logit_roc) #chance line = randomly flipped a coin to assign 0's and 1's

plot(logit_roc)$optimal  #cutoff = optimal cutoff that maximizes Youden index 
summary(logit_roc)


#try combining MMCRED5
#figure out interpretations

####################################################################33
############# Global ACCTAGE interpretation PDP ##################
#######################################################################
pdp_train <- insurance_t %>%
  select(-c(s_ACCTAGE , s_DDABAL ,s_DEP , s_DEPAMT , s_CHECKS , s_NSFAMT , s_PHONE , s_TELLER , s_SAVBAL , s_ATMAMT ,s_POS , s_POSAMT , s_CDBAL , s_IRABAL ,s_INVBAL , s_MMBAL ,s_CCBAL , s_INCOME , s_LORES , s_HMVAL ,s_AGE , s_CRSCORE))

set.seed(12345)
xgb_pred <- Predictor$new(xgb.ins, data = pdp_train, 
                             y = train_y, type = "response")

pd_plot <- FeatureEffects$new(xgb_pred, method = "pdp")
pd_plot$plot(c("ACCTAGE"))

####### alternative method
pdp_acctage <- partial(xgb.ins, pred.var = "ACCTAGE", train = train_x)

plot(pdp_acctage)

