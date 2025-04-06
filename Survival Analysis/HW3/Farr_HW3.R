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
library(zoo)

#################################################################################
############## Reading in the data #########################################
###############################################################################

hurricane <- read.csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv")

View(hurricane)
View(hurricane[,51:62])
str(hurricane)
summary(hurricane)

sum(is.na(hurricane))

#creating event variable
hurricane$pumpFailMotor = ifelse(hurricane$survive != 1 & hurricane$reason ==2, 1, 0)


########################################################################
################ Dealing with Quasi Complete separation ################
#######################################################################

table(hurricane$pumpFailMotor, hurricane$trashrack) #flag
table(hurricane$pumpFailMotor, hurricane$bridgecrane) #flag
table(hurricane$pumpFailMotor, hurricane$backup)
table(hurricane$pumpFailMotor, hurricane$servo)
table(hurricane$pumpFailMotor, hurricane$gear) #flag
table(hurricane$pumpFailMotor, hurricane$elevation) #flag

median(hurricane$slope)
median(hurricane$age)
hurricane$elevation <- ifelse(hurricane$elevation %in% c(2,3), "<=3", ">3")

#removing trashrack variable for quasi complete separation concerns
hurricane <- hurricane %>%
  select(-c(trashrack))

#add 1 observation for trashrack 1 & pumpFailMotor 1
#hurricane[nrow(hurricane) + 1,] <- list(0, 7, 1, 1, 0, 1, 2, "<=3", 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
 #                                           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ,1 ,1, 1 ,1, 1, 0, 48, 2, 2, 1)
#############################################################################################
############### Backward selection #############################################
##################################################################################

full.model <- coxph(Surv(hour, pumpFailMotor) ~ factor(backup) + age + factor(bridgecrane) + factor(servo) + factor(gear) + slope + factor(elevation), 
                    data = hurricane)

back.model <- step(full.model, direction = "backward",
                   k = qchisq(0.03, 1, lower.tail = FALSE))
summary(back.model)

###########################################################################
############ Testing assumptions #######################################
###########################################################################

pump.mod <- coxph(Surv(hour, pumpFailMotor) ~ sin(age) + log(slope+1), data = hurricane)

#Linearity Assumption - Martingale residuals
#only fit continuous variables here
pump.lin <- coxph(Surv(hour, pumpFailMotor) ~  sin(age) + log(slope+1), data = hurricane)
survminer::ggcoxfunctional(pump.lin,data=hurricane)
pump.lin

#Proportional Hazards Assumption - Schoenfeld Residuals
pump.ph.zph <- cox.zph(pump.mod, transform = "identity")
pump.ph.zph ## ALL VARIABLES PASS ASSUMPTION!

ggcoxzph(pump.ph.zph)#p -values are better than the graphs


######################################################################
################### Transforming to start/stop + long format #######################
####################################################################

#create unique id for each pump
hurricane$id <- seq(1, nrow(hurricane))

#pivots large H1-H48 data frame to long format
test <- hurricane %>%
  pivot_longer(cols = c('h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8',
                        'h9', 'h10', 'h11', 'h12', 'h13', 'h14', 'h15', 'h16',
                        'h17', 'h18', 'h19', 'h20', 'h21', 'h22', 'h23', 'h24',
                        'h25', 'h26', 'h27', 'h28', 'h29', 'h30', 'h31', 'h32',
                        'h33', 'h34', 'h35', 'h36', 'h37', 'h38', 'h39', 'h40',
                        'h41', 'h42', 'h43', 'h44', 'h45', 'h46', 'h47', 'h48'),
                        names_to = "hour_status", values_to = "indicator")

#replacing NA values of pump status (h1 - h48) to 0 since it's not working.
#Assuming it is off
sum(is.na(test$indicator))
test$indicator[is.na(test$indicator)] <- 0

#re-formatting the data to get it in long format
#getting a count of the hours from 1 - 48
test$hour_status<-gsub("h","",as.character(test$hour_status))
test$hour_status <- as.numeric(test$hour_status)

#create start stop columns hour by hour
test$start = lag(test$hour_status)
test$start = ifelse(test$hour_status ==1, 0, test$start)
test$stop = test$hour_status

#uses rolling window to see if pump was on in the last 12 hours
#creates indicator 1/0 if it was or not. can't be a 1 for first 12 hours
test$last12 <- rollsumr(test$indicator, k = 12, fill =0)
test$last12Indicator <- ifelse((test$stop < 12)| (test$last12 != 12), 0, 1)

#removing any rows after pump stopped working
#flags any rows after pump stopped working, will remove where ==1
test$pumpStop <- ifelse(test$hour_status > test$hour, 1, 0)

test <- test %>%
  filter(pumpStop != 1)


#####################################################################
############ Cox PH Model ##########################################
####################################################################
pump.long.ph <- coxph(Surv(start, stop, pumpFailMotor) ~ sin(age) + log(slope+1) + last12Indicator, data = test)
summary(pump.long.ph)

(exp(coef(pump.long.ph))-1)*100 #interpretation












#####################################################################
###################################################################
############### don't worry about this - 12 hour blocks code #############
#######################################################################
#########################################################################

#essentially creates an indicator for where the start/stop breaks will be
test$laggy <- lag(test$indicator)
test$xx <- ifelse((test$indicator != test$laggy) | (test$hour_status == 1), 1, 0)
test$hour_48 <- 49

test <- test %>%
  filter(xx == 1) %>%
  select(backup:indicator, hour_48)


#creates start/stop columns
test <- test %>%
  group_by(id) %>%
  mutate(start = hour_status, stop = lead(hour_status, default= hour_48[1])) %>%
  select(backup:id, indicator, start, stop)


#########################################################################
################### Accounting for 12 hour period ########################
########################################################################

#figuring out difference b/w start and stop & how many 12 hour periods there will be
test$diff <- test$stop - test$start
test$num12 <- ceiling(test$diff/12)

#uncount to duplicate rows based on number of 12 hour periods there are
test <- test %>%
  uncount(num12)

#create 12 hour blocks
test <- test %>%
  group_by(id, diff, indicator, start) %>%
  mutate(id_12 = row_number()*12)


#create new start/stop based on conditions below
test$new_stop <- ifelse(test$diff < 12, test$stop, 
                        ifelse(test$stop < test$id_12, test$stop, 
                               ifelse((test$start + test$id_12-1)>49, 49, test$start + test$id_12)))

test$new_start <- ifelse(test$diff < 12, test$start, 
                         ifelse(test$id_12 == 12, test$start, lag(test$new_stop,1)))

#fixes cases where new_stop was greater than stop
test$new_stop <- ifelse(test$new_stop > test$stop, test$stop, test$new_stop)

#creates 12 hour difference
test$new_diff <- test$new_stop - test$new_start

#Logic for start and stop columns:

#if diff < 12, then use original start/stop
#else diff >= 12, then create new start stop
 ###  new_start = (if id_12 = 12, then new_start = start) else start + 12
 ###  new_stop = (if stop < id_12 then stop else start+12)

#create new column for motor failures & pump was running in last 12 hours
test <- test %>%
  select(backup:indicator, start, stop, diff, new_start,new_stop, new_diff) %>%
  mutate(motor_fail = ifelse(pumpFailFlood == 1 & indicator == 1 & new_diff == 12, 1,0))

#######################################################################
############# Cox Proportional Hazard Model #########################
#######################################################################

pump.long.ph <- coxph(Surv(new_start, new_stop, motor_fail) ~ log(age+1) + log(slope+1) + servo + trashrack, data = test)
summary(pump.long.ph)

(exp(coef(pump.long.ph))-1)*100 #interpretation


#do we have to exponentiate to counteract the log transformation
# how do we interpret the log trasnform
# is the test dataset