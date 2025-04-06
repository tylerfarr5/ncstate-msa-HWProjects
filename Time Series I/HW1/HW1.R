library(tseries)
library(forecast)
library(tidyverse)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(ggplot2)
library(seasonalview)
library(aTSA)
library(imputeTS)
library(stringr)
library(ggfortify)
library(lubridate)

elec <- read.csv("https://raw.githubusercontent.com/sjsimmo2/TimeSeries/master/AEP_hourly.csv")

View(elec)
str(elec)

#############################################
######## Q1 #####################################
#################################################

elec$newdate <- mdy_hm(elec$datetime_beginning_ept)
elec$month <- month(elec$newdate)
elec$year <- year(elec$newdate)
elec$day <- day(elec$newdate)

elec$testdate <- paste(elec$month, elec$day, elec$year, sep = '/')
elec$testdate <- as.Date(elec$testdate, format = "%m/%d/%Y")

new.df <- data.frame(elec$testdate, elec$mw)

q1 <- new.df %>%
  group_by(month(elec.testdate), year(elec.testdate)) %>%
  summarize(mean= mean(elec.mw))

q1 <- q1 %>%
  arrange(q1$`year(elec.testdate)`, q1$`month(elec.testdate)`)

q1
median(q1$mean)

##################################################
########### Q2 #####################################
####################################################

energy <- ts(q1$mean, start = 2016, frequency = 12)

decomp_stl <- stl(energy, s.window = 15)
plot(decomp_stl)


#######################################################
############### Q3 ##############################
###########################################

autoplot(energy) + 
  labs (x = "Date", y = "Average MW", title = "Time Series Plot for Average MW") + 
  theme_classic()

#######################################################
############### Q4 ##############################
###########################################

#trend F val
Ft=max(0,1-var(decomp_stl$time.series[,3])/(var(decomp_stl$time.series[,3])+var(decomp_stl$time.series[,2]) + 2*cov(decomp_stl$time.series[,3],decomp_stl$time.series[,2])))
Ft
#seasonal F val
Fs=max(0,1-var(decomp_stl$time.series[,3])/(var(decomp_stl$time.series[,3])+var(decomp_stl$time.series[,1])+2*cov(decomp_stl$time.series[,3],decomp_stl$time.series[,1])))
Fs
