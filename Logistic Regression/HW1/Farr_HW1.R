###############################################
########### Notes #########################
#######################################

#When calling the unique function for some binary variables, some are 0 1 and others are 1 0



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
insurance_t <- read.csv("insurance_t.csv")

#exploring
View(insurance_t)
str(insurance_t)
summary(insurance_t)

#transforming categorical variables w/ 10 levels or less to factor
insurance_t$DDA <- as.factor(insurance_t$DDA)
insurance_t$CASHBK <- as.factor(insurance_t$CASHBK)
insurance_t$DIRDEP <- as.factor(insurance_t$DIRDEP)
insurance_t$NSF <- as.factor(insurance_t$NSF)
insurance_t$SAV <- as.factor(insurance_t$SAV)
insurance_t$ATM <- as.factor(insurance_t$ATM)
insurance_t$CD <- as.factor(insurance_t$CD)
insurance_t$IRA <- as.factor(insurance_t$IRA)
insurance_t$LOC <- as.factor(insurance_t$LOC)
insurance_t$INV <- as.factor(insurance_t$INV)
insurance_t$ILS <- as.factor(insurance_t$ILS)
insurance_t$MM <- as.factor(insurance_t$MM)
insurance_t$MMCRED <- as.factor(insurance_t$MMCRED)
insurance_t$MTG <- as.factor(insurance_t$MTG)
insurance_t$CC <- as.factor(insurance_t$CC)
insurance_t$CCPURC <- as.factor(insurance_t$CCPURC)
insurance_t$SDB <- as.factor(insurance_t$SDB)
insurance_t$HMOWN <- as.factor(insurance_t$HMOWN)
insurance_t$MOVED <- as.factor(insurance_t$MOVED)
insurance_t$INAREA <- as.factor(insurance_t$INAREA)
insurance_t$BRANCH <- as.factor(insurance_t$BRANCH)
insurance_t$RES <- as.factor(insurance_t$RES)
insurance_t$INS <- as.factor(insurance_t$INS)


#########################################################
################# Question 1 ###########################
#######################################################

############### Binary predictors ###############
#Significant: DDA, DIRDEP, NSF, SAV, ATM, CD, IRA, INV, MM, CC, SDB, INAREA
#Not significant: LOC, ILS, MTG, HMOWN, MOVED

#DDA
CrossTable(insurance_t$INS, insurance_t$DDA, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$DDA)) #significant, < 2.2e-16
CMHtest(table(insurance_t$INS, insurance_t$DDA))$table[1,] #linear association, < 2.2e-16
#DIRDEP
CrossTable(insurance_t$INS, insurance_t$DIRDEP, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$DIRDEP)) #significant, 4.33e-11
CMHtest(table(insurance_t$INS, insurance_t$DIRDEP))$table[1,] #linear association, 3.666843e-11
#NSF
CrossTable(insurance_t$INS, insurance_t$NSF, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$NSF)) #significant, 6.464e-11
CMHtest(table(insurance_t$INS, insurance_t$NSF))$table[1,] #linear association, 4.934934e-11
#SAV
CrossTable(insurance_t$INS, insurance_t$SAV, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$SAV)) #significant, < 2.2e-16
CMHtest(table(insurance_t$INS, insurance_t$SAV))$table[1,] #linear association, < 2.2e-16
#ATM
CrossTable(insurance_t$INS, insurance_t$ATM, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$ATM)) #significant, < 2.2e-16
CMHtest(table(insurance_t$INS, insurance_t$ATM))$table[1,] #linear association, < 2.2e-16
#CD
CrossTable(insurance_t$INS, insurance_t$CD, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$CD)) #significant, < 2.2e-16
CMHtest(table(insurance_t$INS, insurance_t$CD))$table[1,] #linear association, < 2.2e-16
#IRA
CrossTable(insurance_t$INS, insurance_t$IRA, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$IRA)) #significant, < 2.2e-16
CMHtest(table(insurance_t$INS, insurance_t$IRA))$table[1,] #linear association, < 2.2e-16
#LOC
CrossTable(insurance_t$INS, insurance_t$LOC, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$LOC)) #NOT significant, 0.5297
CMHtest(table(insurance_t$INS, insurance_t$LOC))$table[1,] #NO linear association, 0.4994954 
#INV
CrossTable(insurance_t$INS, insurance_t$INV, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$INV)) #significant, < 2.2e-16
CMHtest(table(insurance_t$INS, insurance_t$INV))$table[1,] #linear association, < 2.2e-16
#ILS
CrossTable(insurance_t$INS, insurance_t$ILS, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$ILS)) #NOT significant, 0.008481
CMHtest(table(insurance_t$INS, insurance_t$ILS))$table[1,] #NO linear association, 0.007265099
#MM
CrossTable(insurance_t$INS, insurance_t$MM, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$MM)) #significant, < 2.2e-16
CMHtest(table(insurance_t$INS, insurance_t$MM))$table[1,] #linear association, < 2.2e-16
#MTG
CrossTable(insurance_t$INS, insurance_t$MTG, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$MTG)) #NOT significant, 0.5637
CMHtest(table(insurance_t$INS, insurance_t$MTG))$table[1,] #NO linear association, 0.5281149
#CC
CrossTable(insurance_t$INS, insurance_t$CC, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$CC)) #significant, < 2.2e-16
CMHtest(table(insurance_t$INS, insurance_t$CC))$table[1,] #linear association, < 2.2e-16
#SDB
CrossTable(insurance_t$INS, insurance_t$SDB, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$SDB)) #significant, 5.425e-10
CMHtest(table(insurance_t$INS, insurance_t$SDB))$table[1,] #linear association, 4.305166e-10
#HMOWN
CrossTable(insurance_t$INS, insurance_t$HMOWN, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$HMOWN)) #NOT significant, 0.9397
CMHtest(table(insurance_t$INS, insurance_t$HMOWN))$table[1,] #NO linear association, 0.91960882 
#MOVED
CrossTable(insurance_t$INS, insurance_t$MOVED, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$MOVED)) #NOT significant, 0.2666
CMHtest(table(insurance_t$INS, insurance_t$MOVED))$table[1,] #NO linear association, 0.2370667 
#INAREA
CrossTable(insurance_t$INS, insurance_t$INAREA, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$INAREA)) #significant, 1.008e-06
CMHtest(table(insurance_t$INS, insurance_t$INAREA))$table[1,] #linear association, 7.450319e-07

############### Ordinal predictors ###############
#Significant: CASHBK, MMCRED, CCPURC
#Not significant: 

#CASHBK - didn't do MH test
CrossTable(insurance_t$INS, insurance_t$CASHBK, expected = TRUE) #assumptions failed
fisher.test(table(insurance_t$INS, insurance_t$CASHBK)) #significant, 0.001193
#MMCRED -  didn't do MH test
CrossTable(insurance_t$INS, insurance_t$MMCRED, expected = TRUE) #assumptions failed
fisher.test(table(insurance_t$INS, insurance_t$MMCRED)) #significant, 1.171e-15
#CCPURC - didn't do MH test
CrossTable(insurance_t$INS, insurance_t$CCPURC, expected = TRUE) #assumptions failed
fisher.test(table(insurance_t$INS, insurance_t$CCPURC)) #significant, 4.497e-12


### Nominal predictors
#Significant: ??BRANCH??
#Not significant: RES

#BRANCH - hard to tell with assumption
CrossTable(insurance_t$INS, insurance_t$BRANCH, expected = TRUE, prop.chisq = FALSE) #assumptions unconfirmed
fisher.test(table(insurance_t$INS, insurance_t$BRANCH)) #
#RES
CrossTable(insurance_t$INS, insurance_t$RES, expected = TRUE) #assumptions passed
chisq.test(table(insurance_t$INS, insurance_t$RES)) #NOT significant, 0.2343


### Continuous predictors
#ACCTAGE, DDABAL, DEP, DEPAMT, CHECKS, NSFAMT, PHONE, TELLER, SAVBAL, ATMAMT, POS, POSAMT,
#CDBAL, IRABAL, LOCBAL, INVBAL, ILSBAL, MMBAL, MTGBAL, CCBAL, INCOME, LORES, HMVAL, AGE,
#CRSCORE

#i'm lazy, don't want to do all of these if not necessary


##see below


######################################################
################# Question 2 #########################
######################################################

#Significant binary variables: DDA, DIRDEP, NSF, SAV, ATM, CD, IRA, INV, MM, CC, SDB, INAREA

OddsRatio(table(insurance_t$INS, insurance_t$DDA)) #0.3750703
OddsRatio(table(insurance_t$INS, insurance_t$DIRDEP)) #0.7119491
OddsRatio(table(insurance_t$INS, insurance_t$NSF)) #0.5554847
OddsRatio(table(insurance_t$INS, insurance_t$SAV)) #1.831167
OddsRatio(table(insurance_t$INS, insurance_t$ATM)) #0.5929966
OddsRatio(table(insurance_t$INS, insurance_t$CD)) #3.427187
OddsRatio(table(insurance_t$INS, insurance_t$IRA)) #3.184809
OddsRatio(table(insurance_t$INS, insurance_t$INV)) #3.472039
OddsRatio(table(insurance_t$INS, insurance_t$MM)) #2.850281
OddsRatio(table(insurance_t$INS, insurance_t$CC)) #1.781347
OddsRatio(table(insurance_t$INS, insurance_t$SDB)) #1.549652
OddsRatio(table(insurance_t$INS, insurance_t$INAREA)) #0.5745768


##########################################################
################ Question 3 #############################
#########################################################

#same process as above?

#Not significant/Assumption PASSES: LOCBAL, MTGBAL, CCBAL, INCOME, LORES, HMVAL, 
        #AGE, CRSCORE

#Significant/Assumption FAILS: ACCTAGE, DDABAL, DEP, DEPAMT, CHECKS, NSFAMT, PHONE, TELLER, SAVBAL, 
            #ATMAMT, POS, POSAMT, CDBAL, IRABAL, INVBAL, ILSBAL, MMBAL

fit.gam <- gam(INS~s(ACCTAGE), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ ACCTAGE, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(DDABAL), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ DDABAL, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(DEP), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ DEP, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(DEPAMT), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ DEPAMT, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(CHECKS), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ CHECKS, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(NSFAMT), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ NSFAMT, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(PHONE), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ PHONE, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(TELLER), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ TELLER, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(SAVBAL), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ SAVBAL, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(ATMAMT), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ ATMAMT, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(POS), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ POS, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(POSAMT), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ POSAMT, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(CDBAL), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ CDBAL, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(IRABAL), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ IRABAL, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(LOCBAL), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ LOCBAL, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(INVBAL), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ INVBAL, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(ILSBAL), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ ILSBAL, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(MMBAL), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ MMBAL, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(MTGBAL), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ MTGBAL, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(CCBAL), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ CCBAL, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(INCOME), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ INCOME, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(LORES), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ LORES, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(HMVAL), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ HMVAL, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(AGE), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ AGE, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')

fit.gam <- gam(INS~s(CRSCORE), data = insurance_t, family = binomial(link = "logit"), method = "REML")
logit.model <- glm(INS ~ CRSCORE, data = insurance_t, family = binomial(link = "logit"))
anova(logit.model, fit.gam, test = 'Chisq')


##########################################################
################# Question 4 ##########################
##########################################################

#count of missing values by variable
colSums(is.na(insurance_t))

#proportional
round(colSums(is.na(insurance_t)/nrow(insurance_t)),2)

#missing values graph
missing.vals <- data.frame(unknowns = colSums(is.na(insurance_t)/nrow(insurance_t)))
missing.vals <- as.data.frame(missing.vals)
missing.vals$indx <- rownames(missing.vals)

#only looking at variables with missing values
mv.sorted <- missing.vals %>%
  filter(unknowns >0)

#bar chart --- reorder function puts bar graph in descending order
ggplot(data = mv.sorted, aes(x = reorder(indx, -unknowns), y = unknowns)) + 
  geom_bar(stat = 'identity', fill = "blue") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust =1)) +
  labs(x = "Missing Value Variables", y = "Proportion of Missing Values", title = "Proportion of Missing Values by Variable")


### Highly correlated variables?
cor(insurance_t[c(1,3:5,7, 10:12, 14, 16:18, 20, 22, 24,26,28,30,33,35,38, 40:43)])

#noticeable correlations: CHECKS/DEP, TELLER/DEP, TELLER/CHECKS, TELLER/DEPAMT, CHECKS/DEPAMT