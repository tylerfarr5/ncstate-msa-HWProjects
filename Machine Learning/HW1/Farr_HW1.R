########################################################################
############# Reading in the data ####################################
######################################################################
library(ggplot2)
library(tidyverse)
library(earth)
library(ROCR)
library(ROCit)
library(mgcv)

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
####################### Question 1 - MARS approach########################
####################################################################

mars2 <- earth(INS ~ ., data = insurance_t, 
               glm = list(family = binomial(link= 'logit')), #add glm argument for binary output
               nfold = 10, trace = 0.5, pmethod = 'cv') #these 3 args are for cross validation

summary(mars2) #will see that this is actually a form of model selection - 
#17 of 60 predictors

evimp(mars2) #variable importance (higher nsubsets the better)

###### ROCit curve
insurance_t$p_hat <- predict(mars2, type = "response")
logit_roc <- rocit(as.vector(insurance_t$p_hat), insurance_t$INS)
plot(logit_roc)

summary(logit_roc)

########################################################################
####################### Question 2 - GAMS approach ####################
####################################################################

gam1 <- mgcv::gam(INS ~ s(ACCTAGE) + s(DDABAL) + s(DEP) + s(DEPAMT) + s(CHECKS) +
                    s(NSFAMT) + s(PHONE) + s(TELLER) + s(SAVBAL) + s(ATMAMT) +
                    s(POS) + s(POSAMT) + s(CDBAL) + s(IRABAL) + s(INVBAL) +
                    s(MMBAL) + s(CCBAL) + s(INCOME) + s(LORES) + s(HMVAL) + s(AGE) +
                    s(CRSCORE)
                  + DDA + DIRDEP + NSF + SAV + ATM + CD + IRA + INV + MM + MMCRED + 
                    CC + CCPURC + SDB + INAREA + BRANCH, 
                  data = insurance_t, method = 'REML', family = binomial(link = 'logit'),
                  select = TRUE)

summary(gam1) #variables to keep
plot(gam1)

###### ROCit curve
insurance_t$p_hat <- predict(gam1, type = "response")
logit_roc <- rocit(as.vector(insurance_t$p_hat), insurance_t$INS)
plot(logit_roc)

summary(logit_roc)
