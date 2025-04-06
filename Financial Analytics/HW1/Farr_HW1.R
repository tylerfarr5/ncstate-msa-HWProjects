library(ggplot2)
library(tidyverse)
library(smbinning)
library(shades)
library(plotly)
##########################################################################
############ Reading in the data ########################################
#########################################################################

accepted <- read.csv("accepted_customers.csv")
rejected <- read.csv("rejected_customers.csv")


###########################################################################
############# Exploring the data #########################################
#########################################################################

View(accepted)
str(accepted)
summary(accepted)
sum(is.na(accepted)) #no NA's

View(rejected)
str(rejected)
summary(rejected)
sum(is.na(rejected)) #no NA's


table(accepted$GB)

#define a "good variable" where those who did not default are 1's, and those who did are 0
accepted$good <- abs(accepted$GB - 1)
table(accepted$good)


#RESID variable has missing values. Imputing Missing with NA's
accepted$RESID <- ifelse(accepted$RESID == "", NA, accepted$RESID)
rejected$RESID <- ifelse(rejected$RESID == "", NA, rejected$RESID)
accepted$PRODUCT <- ifelse(accepted$PRODUCT == "", NA, accepted$PRODUCT)
rejected$PRODUCT <- ifelse(rejected$PRODUCT == "", NA, rejected$PRODUCT)
accepted$PROF <- ifelse(accepted$PROF == "", NA, accepted$PROF)
rejected$PROF <- ifelse(rejected$PROF == "", NA, rejected$PROF)

########################################################################
############ Converting all categorical variables to factor ############
########################################################################

columns <- c(colnames(accepted))
columns <- columns[c(-23, -24, -25)] #remove GB, can't factor it

#Loops through each column & factors anything binary or nominal

#accepted
for (i in columns) {
  if (length(unique(accepted[[i]])) == 2) {
    accepted[[i]] <- as.factor(accepted[[i]])
  } 
}

#rejected
for (i in columns) {
  if (length(unique(rejected[[i]])) == 2) {
    rejected[[i]] <- as.factor(rejected[[i]])
  } 
}

#fixing some variables that fall outside of the loop
accepted$RESID <- as.factor(accepted$RESID)
rejected$RESID <- as.factor(rejected$RESID)

nominal_vars <- c("BUREAU", "CARDS", "CAR", "NAT", "PRODUCT", "PROF", "REGN", "TEL")

#accepted
for (i in nominal_vars) {
    accepted[[i]] <- as.factor(accepted[[i]])
}
#rejected
for (i in nominal_vars) {
  rejected[[i]] <- as.factor(rejected[[i]])
}

str(accepted)
str(rejected)


#adjust CARDS variable has different unique values between accepted and rejected
unique(levels(accepted$CARDS)) #VISA mybank, VISA Others
unique(levels(rejected$CARDS)) #VISA Citibank, VISA Others
unique(levels(accepted$PRODUCT)) 
unique(levels(rejected$PRODUCT)) #has commas
unique(levels(accepted$PROF)) 
unique(levels(rejected$PROF)) #has commas

#same logic for cards, product, prof
rejected <- rejected %>%
  mutate(CARDS = recode_factor(CARDS, "VISA Citibank" = "VISA Citibank/mybank/Others/AmEx", "VISA Others" = "VISA Citibank/mybank/Others/AmEx", "American Express" = "VISA Citibank/mybank/Others/AmEx")) %>%
  mutate(PRODUCT = recode_factor(PRODUCT, "Furniture,Carpet" = "Furniture or Carpet", "Dept. Store,Mail" = "Dept. Store or Mail", "Radio, TV, Hifi" = "Radio or TV or Hifi")) %>%
  mutate(PROF = recode_factor(PROF, "Civil Service, M" = "Civil Service", "Food,Building,Ca" = "Food or Building", "Sea Vojage, Gast" = "Sea Vojage", "State,Steel Ind," = "State or Steel Ind"))

accepted <- accepted %>%
  mutate(CARDS = recode_factor(CARDS, "VISA mybank" = "VISA Citibank/mybank/Others/AmEx", "VISA Others" = "VISA Citibank/mybank/Others/AmEx", "American Express" = "VISA Citibank/mybank/Others/AmEx")) %>%
  mutate(PRODUCT = recode_factor(PRODUCT, "Furniture,Carpet" = "Furniture or Carpet", "Dept. Store,Mail" = "Dept. Store or Mail", "Radio, TV, Hifi" = "Radio or TV or Hifi")) %>%
  mutate(PROF = recode_factor(PROF, "Civil Service, M" = "Civil Service", "Food,Building,Ca" = "Food or Building", "Sea Vojage, Gast" = "Sea Vojage", "State,Steel Ind," = "State or Steel Ind"))


#########################################################################
######## Train/test split ##############################################
##########################################################################

set.seed(43770)
train_id <- sample(seq_len(nrow(accepted)), size = floor(0.7*nrow(accepted)))

train <- accepted[train_id, ]
test <- accepted[-train_id, ]


#######################################################################
########### Binning the categorical variables ########################
#######################################################################


#####################################
############### Creating bins for continuous/interval variables
######################################

numeric <- colnames(train[,sapply(train,is.numeric)])

allBins <- c()
 #generates possible bins for numeric vars
for (i in numeric){
  allBins[[i]] = smbinning(df = train, y = "good", x = i) 
}

#plots of AGE variable
smbinning.plot(allBins$AGE,option="dist",sub="AGE")
smbinning.plot(allBins$AGE,option="goodrate",sub="AGE")
smbinning.plot(allBins$AGE,option="badrate",sub="AGE")

smbinning.sumiv.plot(smbinning.sumiv(df = train, y = "good"))


#test if IV > 0.1, and then generates binned column
#only select 1:9 b/c 10,11,12 are the weights + predictors
for (j in numeric[1:9]) {
  if (is.na(allBins[[j]]["iv"])) {
    next
  } else if (allBins[[j]]["iv"] >= 0.1) {
    train <- smbinning.gen(df = train, ivout = allBins[[j]], chrname = paste(j, "_bin", sep = ""))
  }
}

#####################################
############### Creating bins for factor variables
######################################

factors <- colnames(train[,sapply(train,is.factor)])
factors <- factors[1:13] #only want the first 13 variables (don't want binned vars)
factors <- append(factors, "RESID")


allBins.factor <- c()

for (i in factors){
  allBins.factor[[i]] = smbinning.factor(df = train, y = "good", x = i) 
}


#test if IV > 0.1, and then generates binned column
for (j in factors) {
  if (allBins.factor[[j]]["iv"] >= 0.1) {
    train <- smbinning.factor.gen(df = train, ivout = allBins.factor[[j]], chrname = paste(j, "_bin", sep = ""))
  }
}


####################################################################
############ Logistic Regression Model
####################################################################

initial_score <- glm(data = train, good ~ AGE_bin + TMJOB1_bin +INCOME_bin +
                       PERS_H_bin + EC_CARD_bin + CARDS_bin,
                     weights = train$X_freq_, family = 'binomial')

summary(initial_score)


######################################################################
####### Backward Selection on Log Reg Model (pval = .005)
######################################################################

empty_model <- glm(data = train, good ~1, weights = train$X_freq_, family = 'binomial')

#use 0.005 to adjust for sample size
backward.model.pval <- step(initial_score, scope = list(lower = empty_model, upper = initial_score), 
                           direction = "backward", 
                           k = qchisq(0.005, 1, lower.tail = FALSE))

#New Model After Backward Selection:

new_score <- glm(data = train, good ~ AGE_bin + TMJOB1_bin +
                   PERS_H_bin + CARDS_bin,
                 weights = train$X_freq_, family = "binomial")

summary(new_score)


#Checking multicollinearity and using Likelihood Ratio Test (LRT)
car::vif(new_score) #all < 5
car::Anova(new_score, test = 'LR', type = 'III') #all sig

#######################################################################
########## Model Evaluation - Pre Reject Inference
######################################################################

train$pred <- new_score$fitted.values

#full list of metrics
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "good", report = 1)

#plots
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "good", report = 0, plot = "ks")

#AUC
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "good", report = 0, plot = "auc")



#######################################################################
########## Model Scorecard - Pre Reject Inference
######################################################################

bin_model <- smbinning.scaling(new_score, pdo = 50, score = 500, odds = 20)
train_model <- smbinning.scoring.gen(bin_model, dataset = train)

bin_model$minmaxscore

########################################################################
########## Reject Inference 
#######################################################################


###########################################
#### creating bins for continuous vars
###########################################

for (j in numeric[1:9]) {
  if (is.na(allBins[[j]]["iv"])) {
    next
  } else if (allBins[[j]]["iv"] >= 0.1) {
    rejected <- smbinning.gen(df = rejected, ivout = allBins[[j]], chrname = paste(j, "_bin", sep = ""))
  }
}

###########################################
#### creating bins for categorical vars
###########################################

for (j in factors) {
  if (allBins.factor[[j]]["iv"] >= 0.1) {
    rejected <- smbinning.factor.gen(df = rejected, ivout = allBins.factor[[j]], chrname = paste(j, "_bin", sep = ""))
  }
}



###########################################################################
########## Model Evaluation - Post Reject Inference
##########################################################################

#creating binned variables for rejected dataset
rejects_scored <- smbinning.scoring.gen(bin_model, dataset = rejected)

#providing predicted probabilities on scored_rejected dataset
rejects_scored$pred <- predict(new_score, rejects_scored, type = "response")

#creating good/weight/GB columns in original rejected dataset, based on scored_rejected set
rejected$good <- as.numeric(rejects_scored$pred > 0.9653) ##YOUDENS INDEX)
rejected$X_freq_ <- ifelse(rejected$good == 1, 20, 0.67) #need to calculate
rejected$GB <- abs(rejected$good - 1)


#removing bins in accepted dataset
rejected <- rejected %>%
  select(-PERS_H_bin, -AGE_bin, -TMJOB1_bin, -INCOME_bin, -EC_CARD_bin, -CARDS_bin)

comb_hard <- rbind(accepted, rejected)


#######################################################################
########## Final Model Scorecard
######################################################################

#####################################
############### train/test split
######################################

set.seed(43770)
train_id <- sample(seq_len(nrow(comb_hard)), size = floor(0.7*nrow(comb_hard)))

train <- comb_hard[train_id, ]
test <- comb_hard[-train_id, ]



#####################################
############### Creating bins for continuous/interval variables
######################################

numeric <- colnames(train[,sapply(train,is.numeric)])

allBins <- c()
#generates possible bins for numeric vars
for (i in numeric){
  allBins[[i]] = smbinning(df = train, y = "good", x = i) 
}

#plots of AGE variable
smbinning.plot(allBins$AGE,option="dist",sub="AGE")
smbinning.plot(allBins$AGE,option="goodrate",sub="AGE")
smbinning.plot(allBins$AGE,option="badrate",sub="AGE")

smbinning.sumiv.plot(smbinning.sumiv(df = train, y = "good"))

#IV Summary
smbinning.sumiv(df = train, y = "good")


#test if IV > 0.1, and then generates binned column
#only select 1:9 b/c 10,11,12 are the weights + predictors
for (j in numeric[1:9]) {
  if (is.na(allBins[[j]]["iv"])) {
    next
  } else if (allBins[[j]]["iv"] >= 0.1) {
    train <- smbinning.gen(df = train, ivout = allBins[[j]], chrname = paste(j, "_bin", sep = ""))
  }
}

#####################################
############### Creating bins for factor variables
######################################

factors <- colnames(train[,sapply(train,is.factor)])
factors <- factors[1:13] #only want the first 13 variables (don't want binned vars)
factors <- append(factors, "RESID")


allBins.factor <- c()

for (i in factors){
  allBins.factor[[i]] = smbinning.factor(df = train, y = "good", x = i) 
}


#test if IV > 0.1, and then generates binned column
for (j in factors) {
  if (is.na(allBins.factor[[j]]["iv"])) {
    next
  } else if (allBins.factor[[j]]["iv"] >= 0.1) {
    train <- smbinning.factor.gen(df = train, ivout = allBins.factor[[j]], chrname = paste(j, "_bin", sep = ""))
  }
}


#############################################
############ Logistic Regression Model
##########################################

initial_score <- glm(data = train, good ~ AGE_bin + TMJOB1_bin +TMADD_bin +
                       PERS_H_bin + EC_CARD_bin + CARDS_bin + CHILDREN_bin,
                     weights = train$X_freq_, family = 'binomial')

summary(initial_score)


############################################
####### Backward Selection on Log Reg Model (pval = .005)
#########################################

empty_model <- glm(data = train, good ~1, weights = train$X_freq_, family = 'binomial')

#use 0.005 to adjust for sample size
backward.model.pval <- step(initial_score, scope = list(lower = empty_model, upper = initial_score), 
                            direction = "backward", 
                            k = qchisq(0.005, 1, lower.tail = FALSE))

#New Model After Backward Selection:

new_score <- glm(data = train, good ~ AGE_bin + TMJOB1_bin + TMADD_bin +
                   PERS_H_bin + CARDS_bin,
                 weights = train$X_freq_, family = "binomial")

summary(new_score)


#Checking multicollinearity and using Likelihood Ratio Test (LRT)
car::vif(new_score) #all < 5
car::Anova(new_score, test = 'LR', type = 'III') #all sig



#####################################
########## Final Model Evaluation
###################################

train$pred <- new_score$fitted.values

#full list of metrics
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "good", report = 1)

#plots
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "good", report = 0, plot = "ks")

#AUC
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "good", report = 0, plot = "auc")


################################################
##### creating final scores
#################################################

bin_model <- smbinning.scaling(new_score, pdo = 50, score = 500, odds = 20)
train_model <- smbinning.scoring.gen(bin_model, dataset = train)

bin_model$minmaxscore



#######################################################################
########## Evaluating on test data 
########################################################################



###########################################
#### creating bins for continuous vars
###########################################

for (j in numeric[1:9]) {
  if (is.na(allBins[[j]]["iv"])) {
    next
  } else if (allBins[[j]]["iv"] >= 0.1) {
    test <- smbinning.gen(df = test, ivout = allBins[[j]], chrname = paste(j, "_bin", sep = ""))
  }
}

###########################################
#### creating bins for categorical vars
###########################################

for (j in factors) {
  if (allBins.factor[[j]]["iv"] >= 0.1) {
    test <- smbinning.factor.gen(df = test, ivout = allBins.factor[[j]], chrname = paste(j, "_bin", sep = ""))
  }
}

######################################
########## Model Scoring on test data
######################################

#creating binned variables for test dataset
test_scored <- smbinning.scoring.gen(bin_model, dataset = test)

#providing predicted probabilities on scored_rejected dataset
test_scored$pred <- predict(new_score, test_scored, type = "response")


#####################################
#### Evaluating the model 
####################################

smbinning.metrics(dataset = test_scored, prediction = "pred", actualclass = "good", report = 1)

smbinning.metrics(dataset = test_scored, prediction = "pred", actualclass = "good", report = 0, plot = "ks")

smbinning.metrics(dataset = test_scored, prediction = "pred", actualclass = "good", report = 0, plot = "auc")



########################################################################
########## Business Evaluation - need to flip axes or redefine GB
#######################################################################

#combine train/test for evaluation
accepts_scored_comb <- rbind(train_model, test_scored)


# Decile Default Plot #make sure you adjust 4.75 to 30!!
cutpoints <- quantile(accepts_scored_comb$Score, probs = seq(0,1,0.10))
accepts_scored_comb$Score.QBin <- cut(accepts_scored_comb$Score, breaks=cutpoints, include.lowest=TRUE)
Default.QBin.pop <- round(table(accepts_scored_comb$Score.QBin, accepts_scored_comb$GB)[,2]/(table(accepts_scored_comb$Score.QBin, accepts_scored_comb$GB)[,2] + table(accepts_scored_comb$Score.QBin, accepts_scored_comb$GB)[,1]*30)*100,2)

print(Default.QBin.pop)

barplot(Default.QBin.pop, 
        main = "Default Decile Plot", 
        xlab = "Deciles of Scorecard",
        ylab = "Default Rate (%)", ylim = c(0,25),
        col = saturation(heat.colors, scalefac(0.8))(10))
abline(h = 3.23, lwd = 2, lty = "dashed")
text(11, 6, "Current = 3.23%")




#################### PROFIT

def = NULL
acc = NULL
prof = NULL
score = NULL

cost = 52000
profit = 2000

for(i in min(floor(accepts_scored_comb$Score)):max(floor(accepts_scored_comb$Score))){
  score[i - min(floor(accepts_scored_comb$Score)) + 1] = i
  def[i - min(floor(accepts_scored_comb$Score)) + 1] = 100*sum(accepts_scored_comb$GB[which(accepts_scored_comb$Score >= i)])/(length(accepts_scored_comb$GB[which(accepts_scored_comb$Score >= i & accepts_scored_comb$GB == 1)]) + 30*length(accepts_scored_comb$GB[which(accepts_scored_comb$Score >= i & accepts_scored_comb$GB == 0)]))
  acc[i - min(floor(accepts_scored_comb$Score)) + 1] = 100*(length(accepts_scored_comb$GB[which(accepts_scored_comb$Score >= i & accepts_scored_comb$GB == 1)]) + 30*length(accepts_scored_comb$GB[which(accepts_scored_comb$Score >= i & accepts_scored_comb$GB == 0)]))/(length(accepts_scored_comb$GB[which(accepts_scored_comb$GB == 1)]) + 30*length(accepts_scored_comb$GB[which(accepts_scored_comb$GB == 0)]))
  prof[i - min(floor(accepts_scored_comb$Score)) + 1] = length(accepts_scored_comb$GB[which(accepts_scored_comb$Score >= i & accepts_scored_comb$GB == 1)])*(-cost) + 30*length(accepts_scored_comb$GB[which(accepts_scored_comb$Score >= i & accepts_scored_comb$GB == 0)])*profit
}


plotData = data.frame(def, acc, prof, score)

def_plot = xyplot(def ~ score, plotData,
                  type = "l" , lwd=2, col="red",
                  ylab = "Default Rate (%)",
                  xlab = "Score",
                  main = "Default Rate by Acceptance Across Score",
                  panel = function(x, y,...) {
                    panel.xyplot(x, y, ...)
                    panel.abline(h = 3.23, col = "red")
                  })

acc_plot = xyplot(acc ~ score, plotData, 
                  type = "l", lwd=2, col="blue",
                  ylab = "Acceptance Rate (%)",
                  panel = function(x, y,...) {
                    panel.xyplot(x, y, ...)
                    panel.abline(h = 75, col = "blue")
                  })

prof_plot = xyplot(prof/1000000 ~ score, plotData,
                   type = "l" , lwd=2, col="green",
                   ylab = "Profit (Millions $)",
                   xlab = "Score",
                   main = "Profit by Acceptance Across Score")

doubleYScale(def_plot, acc_plot, add.ylab2 = TRUE, use.style=FALSE)
doubleYScale(prof_plot, acc_plot, add.ylab2 = TRUE, use.style=FALSE)
