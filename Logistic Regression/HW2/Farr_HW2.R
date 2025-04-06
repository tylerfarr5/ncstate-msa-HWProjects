############################################
############ Questions #########################
#################################################

# Should we add a missing values column to interact with missing categories
#testing for influential observations?


################################################
################ Reading in data #################
####################################################

#reading in the data and libraries
library(ggplot2)
library(tidyverse)
library(gmodels)
library(vcdExtra)
library(mgcv)
library(DescTools)
insurance_t <- read.csv("insurance_t_bin.csv")

#exploring
View(insurance_t)
str(insurance_t)
summary(insurance_t)

#replacing missing values, transforming all variables to factor
insurance_t <- insurance_t %>% 
  replace(is.na(.), "Missing")

col_names <- names(insurance_t)
insurance_t[,col_names] <- lapply(insurance_t[,col_names] , factor)

#############################################################
################ Checking for separation concerns #################
################################################################

vars <- names(insurance_t)

for (i in vars) {
  print(i)
  print(table(insurance_t$INS, insurance_t[,i]))
}

#CASHBK, MMCRED

#CASHBK
insurance_t <- insurance_t %>%
  mutate(CASHBK = ifelse(CASHBK %in% c("1","2"), "1+", "0"))

#MMCRED
insurance_t <- insurance_t %>%
  mutate(MMCRED = ifelse(MMCRED %in% c("3", "4", "5"), "3+", ifelse(MMCRED == "2", "2", ifelse(MMCRED == "1", "1", ifelse(MMCRED == "0", "0", "0")))))

insurance_t$CASHBK <- as.factor(insurance_t$CASHBK)
insurance_t$MMCRED <- as.factor(insurance_t$MMCRED)

#############################################################
################ Main Effects #### #################
################################################################

### remove variables that aren't significant from part 1

insurance_t <- insurance_t %>%
  select(-c(ILS, MOVED, LOC, MTG, HMOWN, RES, CCBAL_Bin, ACCTAGE_Bin, 
            TELLER_Bin, ILSBAL_Bin, INVBAL_Bin, MTGBAL_Bin, POSAMT_Bin, AGE_Bin,
            INCOME_Bin, CRSCORE_Bin, LORES_Bin, LOCBAL_Bin))

### check for multicollinearity

full.mod <- glm(INS ~ . - MMBAL_Bin - BRANCH - POS_Bin - PHONE_Bin - CCPURC
                - DDA - SAV - CD, data = insurance_t, family = binomial(link = "logit"))

alias(full.mod) #any 1's / -1's were flagged for perfect collinearity
summary(full.mod) #any NA values were removed

car::vif(full.mod) #DDA, CD, SAV were recommended to be removed GVIF> 5

### remove variables thru backward selection

back.mod <- step(full.mod, direction= "backward", k = qchisq(0.002, 1, lower.tail = FALSE))

#Final Model Selected:
###   INS ~ DIRDEP + IRA + INV + MM + CC + DDABAL_Bin + CHECKS_Bin + 
###   SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin

summary(back.mod)
AIC(back.mod)
car::Anova(back.mod, test = "LR", type = "III")

#############################################################
################ Interpreting Odds Ratios ######################
################################################################

exp(
  cbind(coef(back.mod), confint(back.mod))
)

## People with a money market account are 2.24 times the odds to buy the insurance
#product as compared to people without a money market account

#############################################################
################ Forward Selection w Interactions ######################
################################################################

full.model <- glm(INS ~ .^2, data = insurance_t, family = binomial(link = "logit"))


for.model <- step(back.mod, scope = list(lower=formula(back.mod), upper=formula(full.model)), 
                  direction = "forward", k = qchisq(0.002, 1, lower.tail = FALSE))

#No interaction terms in this model - they don't add value...
