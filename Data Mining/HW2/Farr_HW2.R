#Data Mining HW

library(rpart)
library(ggplot2)
library(rpart.plot)
library(TH.data)
library(lattice)
library(stats)
library(rattle)
library(RColorBrewer)
library(caret)
library(ROCR)
####################################################################
################# Reading in the data ############################
####################################################################

train <- read.csv("https://raw.githubusercontent.com/sjsimmo2/DataMining-Fall/master/insurance_t.csv")
valid <- read.csv("https://raw.githubusercontent.com/sjsimmo2/DataMining-Fall/master/insurance_v.csv")

str(train)
summary(train)

summary(valid)
str(valid)
######################################################################
################ Decision Tree #######################################
####################################################################

BC.tree <- rpart(INS ~ . , data=train, method='class',
                parms = list(split='gini'),
                control = rpart.control(cp = 0.003, maxdepth = 5, minsplit = 130)) 

summary(BC.tree)
print(BC.tree)

################################################################
################ visual of classification tree + VarImp Plots ##############
###############################################################

rpart.plot::rpart.plot(BC.tree)

#Var Imp Plot
varimp.data <- data.frame(BC.tree$variable.importance)
varimp.data$names <- as.character(rownames(varimp.data))

ggplot(data=varimp.data,aes(x=reorder(names, BC.tree.variable.importance),y=BC.tree.variable.importance))+
  geom_bar(stat="identity", fill = "blue")+
  coord_flip()+
  labs(x="Variable Name",y="Variable Importance") +
  theme_classic()

################################################################
##### Evaluating the decision tree + misclassification rates#####
##########################################################
tscores = predict(BC.tree,type='class')
scores = predict(BC.tree, valid, type='class')

##Training misclassification rate:
sum(tscores!= train$INS)/nrow(train) #0.2806357

### Valid misclassification data:
sum(scores!= valid$INS)/nrow(valid) #0.2923729

#### Accuracy on valid data
sum(scores == valid$INS)/nrow(valid) #0.7076271

###################################
########## Lift + ROC Curve #############
###################################

#Lift
scores1=predict(BC.tree,valid,type="prob")
pred_val <- prediction(scores1[,2],valid$INS)
plot(performance(pred_val, measure="lift", x.measure="rpp"), colorize=TRUE)


# Calculating True Positive and False Positive Rate
perf_val <- performance(pred_val, "tpr", "fpr")

#Plot the ROC curve
plot(perf_val)

###################################
########## KS Statistc #############
###################################

#Calculating KS statistic
ks1.tree <- max(attr(perf_val, "y.values")[[1]] - (attr(perf_val, "x.values")[[1]]))
ks1.tree #0.3534293