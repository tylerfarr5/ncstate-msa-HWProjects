#reading in the data and libraries
library(ggplot2)
library(tidyverse)
library(gmodels)
library(vcdExtra)
library(mgcv)
library(DescTools)
library(Hmisc)
library(survival)
library(ROCit)
library(ROCR)

insurance_t <- read.csv("insurance_t_bin.csv")
insurance_v <- read.csv("insurance_v_bin.csv")

#exploring
View(insurance_t)
str(insurance_t)
summary(insurance_t)

View(insurance_v)
str(insurance_v)
summary(insurance_v)

#replacing missing values
insurance_t <- insurance_t %>% 
  replace(is.na(.), "Missing")

insurance_v <- insurance_v %>%
  replace(is.na(.), "Missing")

#################### Quasi Separation Concerns
#CASHBK
insurance_t <- insurance_t %>%
  mutate(CASHBK = ifelse(CASHBK %in% c("1","2"), "1+", "0"))

insurance_v <- insurance_v %>%
  mutate(CASHBK = ifelse(CASHBK %in% c("1","2"), "1+", "0"))

#MMCRED
insurance_t <- insurance_t %>%
  mutate(MMCRED = ifelse(MMCRED %in% c("3", "4", "5"), "3+", ifelse(MMCRED == "2", "2", ifelse(MMCRED == "1", "1", ifelse(MMCRED == "0", "0", "0")))))

insurance_v <- insurance_v %>%
  mutate(MMCRED = ifelse(MMCRED %in% c("3", "4", "5"), "3+", ifelse(MMCRED == "2", "2", ifelse(MMCRED == "1", "1", ifelse(MMCRED == "0", "0", "0")))))

################ transform all variables to factor
col_names <- names(insurance_t)
insurance_t[,col_names] <- lapply(insurance_t[,col_names] , factor)

col_names_v <- names(insurance_v)
insurance_v[,col_names_v] <- lapply(insurance_v[,col_names_v] , factor)

###################################################################
################# Variables from previous model ###################
##################################################################

###   INS ~ DIRDEP + IRA + INV + MM + CC + DDABAL_Bin + CHECKS_Bin + 
###   SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin

final.log.model <- glm(INS ~ DIRDEP + IRA + INV + MM + CC + DDABAL_Bin + 
      CHECKS_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin, data = insurance_t,
      family = binomial(link = "logit"))

summary(final.log.model)

#significant variables list in HW 2 report


###################################################################
################# Probability Metrics - train ###################
##################################################################


####### Discrimination Slope
insurance_t$p_hat <- predict(final.log.model, type = "response")
p1 <- insurance_t$p_hat[insurance_t$INS == 1]
p0 <- insurance_t$p_hat[insurance_t$INS == 0]

coef_discrim <- mean(p1) - mean(p0)

#Difference in avg pred prob b/w 1's and 0's = 0.234. Model doesn't do a great job at differentiating

ggplot(insurance_t, aes(p_hat, fill = INS)) +
  geom_density(alpha = 0.7) +
  scale_fill_grey() + 
  labs(x = "Predicted Probability", fill = "Outcome",
       title = paste("Coefficient of Discrimination: ", round(coef_discrim,3), sep = ""))


######## Concordance
somers2(insurance_t$p_hat, insurance_t$INS) #this isn't working

survival::concordance(final.log.model)
#79.33% of the time, model is able to correctly rank 1's ahead of 0's


###################################################################
################# Classification Metrics - train ###################
##################################################################

###### ROCit curve
logit_roc <- rocit(insurance_t$p_hat, insurance_t$INS)
plot(logit_roc)

summary(logit_roc)


###### More professional looking ROC Curve?

pred <- prediction(insurance_t$p_hat, insurance_t$INS)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, lwd = 3, colorize = FALSE, colorkey = FALSE)
abline(a = 0, b = 1, lty = 3, col = "red")
title("ROC Curve: True Positive Rate vs False Positive Rate\nArea Under Curve: 0.7933")
legend(x = "bottomright", lty = c(1,20), legend = c("Empirical ROC Curve", "Chance Line"), col = c("black", "red"))


###### K-S Statistic
ksplot(logit_roc)

ksplot(logit_roc)$`KS stat` #0.4574263 , Youden Index
ksplot(logit_roc)$`KS Cutoff` #0.3023139, optimal cutoff / largest difference in both cumulative distributions


###################################################################
################# Classification Metrics - validation ###################
##################################################################

####### Final confusion matrix

#gets predictions on validation data
insurance_v$p_hat <- NA
insurance_v$p_hat <- predict(final.log.model, newdata = insurance_v ,type = "response")

#creates optimal cutoff variable
insurance_v <- insurance_v %>%
  mutate(INS_hat = ifelse(p_hat > 0.3023139,1,0)) 

#confusion matrix: compares Actual vs Predicted
table(insurance_v$INS_hat, insurance_v$INS)
# (TP + TN) / n
# Accuracy at cut off of 0.3023139
# This was determined by the K-S in train data
(579 + 937)/ 2124


######## Accuracy

#looks at all classification tables for all values of cutoffs b/w 0 and 1
logit_meas <- measureit(insurance_v$p_hat, insurance_v$INS, measure = c("ACC", "FSCR"))
summary(logit_meas)

#We see accuracy is maximized at 73.6%.
#The pred prob that this occurs is 0.462, as shown by Cutoff column
#So - if model predicts probability > 0.462, then it's a 1; otherwise 0
acc_table <- data.frame(Cutoff = logit_meas$Cutoff, Acc = logit_meas$ACC)
head(arrange(acc_table, desc(Acc)), n = 10)


######## Lift
logit_roc_v <- rocit(insurance_v$p_hat, insurance_v$INS)
logit_lift <- gainstable(logit_roc_v)
print(logit_lift) #ranks highest pred prob to lowest on 10 bins

plot(logit_lift, type = 1) #Lift/Cumulative Lift
title("Lift/Cumulative Lift vs. Population Depth (%)")

plot(logit_lift, type = 2) #Resp Rate/ Cumulative Resp Rate

plot(logit_lift, type = 3) #Cumulative capture rate plot (how many 1's were captured with model) - diagnoal line is 'random', further away the better
title("Gain vs Population Depth (%)")
#Model did 1.877 (=0.656/0.349) times better than random with our top 10% of customers.


