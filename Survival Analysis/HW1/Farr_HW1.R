library(tidyverse)
library(ggplot2)
library(survival)
library(foreign)
library(survminer)
library(rms)
library(flexsurv)
library(dplyr)
library(ciTools)
#library(here)
library(visreg)
library(cmprsk)

#################################################################################
############## Reading in the data #########################################
###############################################################################

hurricane <- read.csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv")

View(hurricane)
View(hurricane[,51:61])
str(hurricane)
summary(hurricane)

sum(is.na(hurricane))

#creating event variable
hurricane$pumpFail = ifelse(hurricane$survive == 1, 0, 1)


################################################################################
################# Give the percentage of pumps within each failure type  #######
##############and percentage of pumps that did not fail. ######################
#############################################################################


hurricane %>%
  group_by(reason) %>%
  summarise(count = n(),
            total = nrow(hurricane),
              percPumps = (n()/nrow(hurricane))*100)

#################################################################################
######### Give the average time until failure for each failure type. ###########
### Are means a good measure for length of survival?  Discuss why or why not ######
###############################################################################

#mean
hurricane %>%
  group_by(reason) %>%
  summarise(count = n(),
            total = nrow(hurricane),
            avgTimeUntilFailure = mean(hour))

#median
hurricane %>%
  group_by(reason) %>%
  summarise(count = n(),
            total = nrow(hurricane),
            avgTimeUntilFailure = median(hour))

###############################################################################
############### Create and upload the survival probability across time for ###
##### pumps broken down by failure type overlaid into one graph. ###############
##############################################################################

# Create a Survival Analysis Object 
hurricane.s=Surv(time=hurricane$hour, event=hurricane$pumpFail)


# Create a Kaplan-Meier Survival Curve with Censoring 
simple_km=survfit(Surv(time = hour, event = pumpFail)~1, data = hurricane)
summary(simple_km)

plot(simple_km, main = "Survival Function", xlab = "Tenure", ylab = "Survival Probability")

ggsurvplot(simple_km, data = hurricane, conf.int = T, palette = "purple", xlab = "Tenure", ylab = "Survival Probability", legend = "none", break.y.by = 0.1)


############## Stratified Analysis
hurricane <- hurricane %>%
  filter(reason != 0)

hurricane.strat = survfit(Surv(hour, pumpFail) ~ reason,data=hurricane)
ggsurvplot(hurricane.strat,data=hurricane,palette = c("orange", "green", "red", "purple"),conf.int = T) +
  ggtitle("Survival Probability vs Time by Failure Type")

##############################################################################
####### Create and upload the graph of conditional failure probabilities across ###
#### time for pumps broken down by failure type overlaid into one graph.#####
###############################################################################

#don't consider reason = 0 here
h= hurricane.strat$n.event/hurricane.strat$n.risk

h1 = h[2:46] #reason =1
h2 = h[47:70] #reason = 2
h3 = h[71:98] #reason = 3
h4 = h[99:119] #reason = 4

index.h1=rep(0,length=(max(hurricane$hour)+1)) #Need to add 0
index.h1[(hurricane.strat$time[2:46])+1]= h1 #Because of 0

index.h2=rep(0,length=(max(hurricane$hour)+1)) #Need to add 0
index.h2[(hurricane.strat$time[47:70])+1]= h2 #Because of 0

index.h3=rep(0,length=(max(hurricane$hour)+1)) #Need to add 0
index.h3[(hurricane.strat$time[71:98])+1]= h3 #Because of 0

index.h4=rep(0,length=(max(hurricane$hour)+1)) #Need to add 0
index.h4[(hurricane.strat$time[99:119])+1]= h4 #Because of 0

haz.plot=data.frame(cbind(seq(0,max(hurricane$hour)), index.h1, index.h2, index.h3, index.h4))
colnames(haz.plot)=c("Time","Hazard1", "Hazard2", "Hazard3", "Hazard4")

ggplot(haz.plot,aes(x=Time))+ #hazard graph of visualizing probabilities
  geom_line(aes(y = Hazard1, color = "Reason 1")) +
  geom_line(aes(y = Hazard2, color = "Reason 2")) +
  geom_line(aes(y = Hazard3, color = "Reason 3")) +
  geom_line(aes(y = Hazard4, color = "Reason 4")) +
  theme_classic() +
  labs(y = "Hazard Probability") +
  scale_color_manual(name = "Pumps", values = c("Reason 1" = "orange", "Reason 2" = "green", "Reason 3" = "red", "Reason 4" = "purple"))+
  ggtitle("Hazard Probability vs Time by Failure Type")

#cumulative hazard overlaid
ggsurvplot(hurricane.strat, data = hurricane, fun = "cumhaz", 
           palette = c("blue","orange", "green", "red", "purple"), 
           conf.int = TRUE, xlab = "Week", 
           ylab = "Cumulative Hazard")

#############################################################################
###### Provide a statistical test to see if the major types of failure ######
####### have similar survival probabilities across time (include null and ######
###### alternative hypotheses, test statistic, p-value and conclusion). #####
#########################################################################

#Risk Table
ggsurvplot(
  hurricane.strat,   data = hurricane,   size = 1,                 
  palette = c("orange", "green", "red", "purple"),
  conf.int = TRUE, pval = TRUE,     
  risk.table = TRUE, risk.table.col = "reason",
  risk.table.height = 0.25,ggtheme = theme_bw() )


survdiff(Surv(hour, pumpFail) ~ reason, data=hurricane, rho=0) 
#reject the null, so at least one survival curve is different