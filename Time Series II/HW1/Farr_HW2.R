library(ggplot2)
library(tidyverse)
library(dplyr)
library(tseries)
library(forecast)
library(zoo)
library(lubridate)
library(aTSA)
library(imputeTS)
library(prophet)
##########################################################################
#################### Reading in the data ################################
################################################################################

# combining any validation sets with train to train on more recent data?
#start point for 1/1/2019 and 1/1/2020-->removed 2019 not relevant

aep_train <- read.csv("hrl_load_metered.csv")
aep_valid <- read.csv("hrl_load_metered - test1.csv") #9/22 - 9/27
aep_valid2 <- read.csv("hrl_load_metered - test2.csv") #9/28 - 10/4
aep_valid3 <- read.csv("hrl_load_metered - test3.csv") #10/5 - 10/11
aep_valid4 <- read.csv("hrl_load_metered - test4.csv") #10/12-10/18

##### Fixing validation3 ################
aep_valid3 <- aep_valid3[c(2:8)]
aep_valid3$date2 <- mdy_hms(aep_valid3$datetime_beginning_ept)

aep_train$date2 <- mdy_hm(aep_train$datetime_beginning_ept)
aep_valid$date2 <- mdy_hm(aep_valid$datetime_beginning_ept)
aep_valid2$date2 <- mdy_hm(aep_valid2$datetime_beginning_ept)
aep_valid4$date2 <- mdy_hm(aep_valid4$datetime_beginning_ept)


aep <- rbind(aep_train, aep_valid, aep_valid2, aep_valid3, aep_valid4)

View(aep)
str(aep)

#converting to date/time object & pulling month and year columns out for TS
#aep$month <- month(aep$date2)
#aep$year <- year(aep$date2)
#aep$hour <- hour(aep$date2)
#aep$day <- day(aep$date2)

#aep_mod <- aep %>%
  #select(hour,day, month, year, mw) %>%
  #arrange(year, month,day,hour)

#start data on 1/1/2020 - index 35605
#1/1/2019 = index 26305
#aep_mod <- aep_mod[35065:nrow(aep_mod),]

#converting to time series object
#energy <- ts(aep_mod$mw, start = c(2020,1), frequency = 24)

#autoplot(energy)
#ggsubseriesplot(energy)

#time series plot that highlights missing values - we see NONE
#ggplot_na_distribution(energy) +
 # labs(y = "Megawatt Usage")

########################################################################
######################### train/test/valid split ########################
#######################################################################

#train = original, valid = week 1 only
#train <- subset(energy, end= length(energy)-312) 
#valid <- subset(energy, start = length(energy)-311, end = length(energy) - 168)

#train = original + week 1,2,3, valid = week 4 only
#train <- subset(energy, end= length(energy)-168) #includes original + week 1,2,3
#valid <- subset(energy, start = length(energy)-167) #week4 data


#######################################################################
######################## Decomposition ##################################
#######################################################################

#decomp_stl <- stl(train, s.window = 7)
#plot(decomp_stl)

#autoplot(train) +
#  theme_classic() +
#  ggtitle("Hourly Energy Usage Over Time (1/1/2020 - 10/11/2023)") +
#  xlab("Time") +
#  ylab("Energy Usage (megawatts)")
#reject all lags so it is stationary about the nonzero mean

########################################################################
######################## Prophet ####################################
######################################################################

aep_prophet <- aep %>%
  select(date2,mw)

aep_prophet <- aep_prophet[35065:nrow(aep_prophet),] #starting on 1/1/2020
aep_prophet_t <- head(aep_prophet, nrow(aep_prophet)-168) #keeping everything besides week 4
aep_prophet_v <- tail(aep_prophet, 168) #week 4 validation
colnames(aep_prophet_t) <- c("ds", "y")

set.seed(6)
Prof <- prophet(changepoint.prior.scale = 0.01)
Prof <- add_country_holidays(Prof, "US")
Prof <- fit.prophet(Prof, aep_prophet_t)
future <- make_future_dataframe(Prof , period = 168, freq = 3600)
fcst <- predict(Prof, future)

plot(Prof, fcst)

#calculate prediction errors
Prophet.error <- aep_prophet_v$mw - tail(fcst$yhat, 168)

# Calculate prediction error statistics (MAE and MAPE)
Prophet.MAE <- mean(abs(Prophet.error))
Prophet.MAPE <- mean(abs(Prophet.error)/abs(aep_prophet_v$mw))*100

Prophet.MAE #135.805
Prophet.MAPE #3.644754


###################### Prophet with intervention (winter storm) ##################
holiday_s <- data.frame(
  holiday = 'winterstorm2022',
  ds = as.Date(c('2022-12-23 11:00:00', '2022-12-23 12:00:00', '2022-12-23 13:00:00',
                 '2022-12-23 14:00:00', '2022-12-23 15:00:00', '2022-12-23 16:00:00',
                 '2022-12-23 17:00:00', '2022-12-23 18:00:00', '2022-12-23 19:00:00',
                 '2022-12-23 20:00:00', '2022-12-23 21:00:00', '2022-12-23 22:00:00', 
                 '2022-12-23 23:00:00', '2022-12-24 00:00:00', '2022-12-24 01:00:00',
                 '2022-12-24 02:00:00', '2022-12-24 03:00:00', '2022-12-24 04:00:00',
                 '2022-12-24 05:00:00', '2022-12-24 06:00:00', '2022-12-24 07:00:00',
                 '2022-12-24 08:00:00', '2022-12-24 09:00:00', '2022-12-24 10:00:00',
                 '2022-12-24 11:00:00')),
  lower_window = 0,
  upper_window = 0
)

prophet.data <- data.frame(ds = seq(as.Date('2020-01-01'), as.Date('2023-10-11'), by = 'h'), y = aep_prophet_t)


#essentially adds components in like ggplot!
Prof <- prophet(holidays = holidays)
Prof <- add_country_holidays(Prof, "US")
Prof <- add_seasonality(Prof, name='monthly', period=30.5, fourier.order=6) #default is 10 Fourier transforms
Prof <- fit.prophet(Prof, prophet.data)

#Forecasting with Prophet model

forecast.data <- make_future_dataframe(Prof, periods = 12, freq = 'month') #must specify freq b/c default is daily
predict(Prof, forecast.data)$yhat

plot(Prof, predict(Prof, forecast.data))

# Calculate prediction errors from forecast
Prophet.error <- test - tail(predict(Prof, forecast.data)$yhat, 12)

# Calculate prediction error statistics (MAE and MAPE)
Prophet.MAE <- mean(abs(Prophet.error))
Prophet.MAPE <- mean(abs(Prophet.error)/abs(test))*100

Prophet.MAE
Prophet.MAPE


####################################################################
################ Neural Network ################################
################################################################