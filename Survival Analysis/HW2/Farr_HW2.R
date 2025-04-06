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
hurricane$pumpFailFlood = ifelse(hurricane$survive != 1 & hurricane$reason ==1, 1, 0)


###############################################################################
########## Distribution Assumption= graphs ######################################################
###############################################################################

#all seem to be fine except Exponential. Statistical tests say use Weibull

#Weibull
hurr.aft.w <- flexsurvreg(Surv(hour, pumpFailFlood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane, dist = "weibull")

plot(hurr.aft.w, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "Cumulative Hazard", main = "Weibull Distribution")

#Exponential
hurr.aft.e <- flexsurvreg(Surv(hour, pumpFailFlood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane, dist = "exp")

plot(hurr.aft.e, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "Cumulative Hazard", main = "Exponential Distribution")

#Gamma
hurr.aft.g <- flexsurvreg(Surv(hour, pumpFailFlood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane, dist = "gamma")

plot(hurr.aft.g, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "Cumulative Hazard", main = "Gamma Distribution")

#Log Logistic
hurr.aft.ll <- flexsurvreg(Surv(hour, pumpFailFlood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane, dist = "llogis")

plot(hurr.aft.ll, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "Cumulative Hazard", main = "Log=Logistic Distribution")

#Log Normal
hurr.aft.ln <- flexsurvreg(Surv(hour, pumpFailFlood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane, dist = "lognormal")

plot(hurr.aft.ln, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "Cumulative Hazard", main = "Log-Normal Distribution")

############################################################################
################## Distribution Assumption - statistics ##################
##########################################################################

pval.e.g = pchisq((-2*(hurr.aft.e$loglik-hurr.aft.g$loglik)), 2,lower.tail=F)
pval.w.g = pchisq((-2*(hurr.aft.w$loglik -hurr.aft.g$loglik)), 1,lower.tail=F)
pval.ln.g = pchisq((-2*(hurr.aft.ln$loglik-hurr.aft.g$loglik)), 1,lower.tail=F)
##pval.g.f = pchisq((-2*(like.g-like.f)), 1,lower.tail=F) 

#simpler distribution on left
Tests = c('Exp vs. Gam', 'Wei vs. Gam', 'LogN vs. Gam')
P_values = c(pval.e.g, pval.w.g, pval.ln.g)
cbind(Tests, P_values)


############################################################################
################ Variable Selection ######################################
############################################################################

#summary(full.model)
full.model <- survreg(Surv(hour, pumpFailFlood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane, dist = "weibull")
empty.model <- survreg(Surv(hour, pumpFailFlood)~1, data = hurricane, dist = "weibull")

back.model <- step(full.model, scope = list(lower=formula(empty.model), 
                                            upper=formula(full.model)),
                   direction = "backward",
                   k = qchisq(0.03, 1, lower.tail = FALSE))
summary(back.model)

(exp(coef(back.model))-1)*100


#############################################################################
################### Which pumps to upgrade #################################
###########################################################################
hurricane$id = seq(1, nrow(hurricane))
survprob.actual = 1 - psurvreg(hurricane$hour,
                               mean = predict(back.model, type = "lp"),
                               scale = back.model$scale, distribution = back.model$dist)

# Predicted Change in Event Time - Backup #
new_time = qsurvreg(1 - survprob.actual,
                    mean = predict(back.model, type = "lp") +
                      coef(back.model)['backup'],
                    scale = back.model$scale,
                    distribution = back.model$dist)

hurricane$new_time = new_time
hurricane$diff = hurricane$new_time - hurricane$hour

impact.backup=data.frame(hurricane$id, hurricane$hour, hurricane$new_time, hurricane$diff,hurricane$pumpFailFlood,hurricane$backup)
colnames(impact.backup)=c("Obs", "O.Hour","N.Hour","Diff","pumpFailFlood","Backup")

head(impact.backup)

impact.backup2=subset(impact.backup,pumpFailFlood==1 & Backup==0)
impact.backup2 %>%
  arrange(desc(Diff))



# Predicted Change in Event Time - Servo#
new_time = qsurvreg(1 - survprob.actual,
                    mean = predict(back.model, type = "lp") +
                      coef(back.model)['servo'],
                    scale = back.model$scale,
                    distribution = back.model$dist)

hurricane$new_time = new_time
hurricane$diff = hurricane$new_time - hurricane$hour

impact.backup=data.frame(hurricane$id, hurricane$hour, hurricane$new_time, hurricane$diff,hurricane$pumpFailFlood,hurricane$servo)
colnames(impact.backup)=c("Obs", "O.Hour","N.Hour","Diff","pumpFailFlood","Servo")

head(impact.backup)

impact.backup2=subset(impact.backup,pumpFailFlood==1 & Servo==0)
impact.backup2%>%
  arrange(desc(Diff))
