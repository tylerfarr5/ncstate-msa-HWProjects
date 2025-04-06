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

## data preparation ####

# training data combining validation 1-4 
aep_train <- read.csv("hrl_load_metered.csv")
aep_valid <- read.csv("hrl_load_metered - test1.csv") #9/22 - 9/27
aep_valid2 <- read.csv("hrl_load_metered - test2.csv") #9/28 - 10/4
aep_valid3 <- read.csv("hrl_load_metered - test3.csv") #10/5 - 10/11
aep_valid4 <- read.csv("hrl_load_metered - test4.csv") #10/12-10/18
aep_valid5 <- read.csv("hrl_load_metered - test5.csv") #10/19 - 10/25

# combine validation week 1,2,3,4,5
aep <- rbind(aep_train, aep_valid, aep_valid2, aep_valid3[ ,c(2:8)], aep_valid4, aep_valid5)
aep <- aep[,c(1,6)]

## create a new df that follow prophet requirement### 
aep_prophet_mod <- aep %>%
  mutate(datetime_beginning_ept = strptime(datetime_beginning_ept, format = "%m/%d/%y %H:%M")) %>%
  mutate(datetime_beginning_ept = format(datetime_beginning_ept, format = '%Y-%m-%d %H:%M:%S')) %>%
  dplyr::select(datetime_beginning_ept, mw)

aep_prophet_mod$datetime_beginning_ept[68016:68183] <- as.character.Date(mdy_hms(aep_valid3$datetime_beginning_ept))

aep_prophet_mod$datetime_beginning_ept[68016] <- "2023-10-05 00:00:00"
aep_prophet_mod$datetime_beginning_ept[68040] <- "2023-10-06 00:00:00"
aep_prophet_mod$datetime_beginning_ept[68064] <- "2023-10-07 00:00:00"
aep_prophet_mod$datetime_beginning_ept[68088] <- "2023-10-08 00:00:00"
aep_prophet_mod$datetime_beginning_ept[68112] <- "2023-10-09 00:00:00"
aep_prophet_mod$datetime_beginning_ept[68136] <- "2023-10-10 00:00:00"
aep_prophet_mod$datetime_beginning_ept[68160] <- "2023-10-11 00:00:00"


colnames(aep_prophet_mod)[1] <- 'ds'
colnames(aep_prophet_mod)[2] <- 'y'

#start data on 1/1/2020 - index 35065
#1/1/2019 = index 26305
aep_prophet_mod <- aep_prophet_mod[35065:nrow(aep_prophet_mod),]
aep <- aep[35065:nrow(aep),]

#removes duplicates from leap years
aep_prophet_mod <- unique.data.frame(aep_prophet_mod)
prophet_train <- aep_prophet_mod[c(1:(nrow(aep_prophet_mod)-168)),]
prophet_valid <- aep_prophet_mod[c((nrow(aep_prophet_mod)-167):(nrow(aep_prophet_mod))),]


energy <- ts(aep_prophet_mod$y, start = c(2020,1), frequency = 24)

train <- subset(energy, end= length(energy)-168) #includes original + week 1,2,3,4
valid <- subset(energy, start = length(energy)-167) #week5 data


## Holt Winter ######################################################################################
######################################################################################################
#use this for the first 2 days... prediction is very low
hwes.energy.m <- hw(train, seasonal = "multiplicative", initial = "optimal", h = 168)

# validation Holt Winter Model
error.hw <- valid - hwes.energy.m$mean
MAPE.hw <- mean(abs(error.hw)/abs(valid))*100
MAPE.hw #6.842734

MAE.hw <- mean(abs(error.hw))
MAE.hw# #275.1091


## ARIMA w/ Intervention ############################################################################
# ARIMA <- Arima(train, order=c(2,0,2), seasonal=c(3,1,2),xreg = spike, method = "CSS") from HW1

#######################################################################################################
# creating intervention variable 12/24/2022, (12/23 12 pm - 12/24 12 pm), large spikes of megawatt usage (BLACKOUTS)
spike <- rep(0, 33287)
spike[c(26101:26125)] <- 1


S.ARIMA <- Arima(train, order=c(2,0,2), seasonal=c(3,1,2),xreg = spike, method = "CSS")

summary(S.ARIMA) 
checkresiduals(S.ARIMA) #shows histogram, Ljung Box test, etc

S.ARIMA %>%
  residuals() %>%
  ggtsdisplay()

#make this spike the length of the validation set
spike2 <- rep(0,168) #create intervention variable for forecasted values

S.ARIMA.error <- valid - forecast::forecast(S.ARIMA, h = 168, xreg = spike)$mean

# Calculate prediction error statistics (MAE and MAPE)
S.ARIMA.MAE <- mean(abs(S.ARIMA.error))
S.ARIMA.MAPE <- mean(abs(S.ARIMA.error)/abs(valid))*100

S.ARIMA.MAE #181.0712
S.ARIMA.MAPE #4.634163



######### Prophet
#######################################################################################################
holiday_s <- data.frame(
  holiday = 'storm2022',
  ds = as.Date(c('2022-12-23 12:00:00', '2022-12-23 13:00:00',
                 '2022-12-23 14:00:00', '2022-12-23 15:00:00', '2022-12-23 16:00:00',
                 '2022-12-23 17:00:00', '2022-12-23 18:00:00', '2022-12-23 19:00:00',
                 '2022-12-23 20:00:00', '2022-12-23 21:00:00', '2022-12-23 22:00:00', 
                 '2022-12-23 23:00:00', '2022-12-24 00:00:00', '2022-12-24 01:00:00',
                 '2022-12-24 02:00:00', '2022-12-24 03:00:00', '2022-12-24 04:00:00',
                 '2022-12-24 05:00:00', '2022-12-24 06:00:00', '2022-12-24 07:00:00',
                 '2022-12-24 08:00:00', '2022-12-24 09:00:00', '2022-12-24 10:00:00',
                 '2022-12-24 11:00:00', '2022-12-24 12:00:00')),
  lower_window = 0,
  upper_window = 0
)

Prof_h <- prophet(holidays = holiday_s, changepoint.prior.scale = 0.01)
Prof_h <- add_country_holidays(Prof_h, "US")
#Prof_h <- add_seasonality(Prof_h, name='hourly', period=1/24, fourier.order=12)
Prof_h <- fit.prophet(Prof_h, prophet_train)

# predict
future_h <- make_future_dataframe(Prof_h, periods = 168, freq = 3600) # predict new week - 168 row
#'day', 'week', 'month', 'quarter', 'year', 1(1 sec), 60(1 minute) or 3600(1 hour).
fcst_h <- predict(Prof_h, future_h)
#plot(Prof_h, fcst_h)

#validation
Prophet.error_h <- valid - tail(fcst_h$yhat, 168)
# Calculate prediction error statistics (MAE and MAPE)
Prophet.MAE.h <- mean(abs(Prophet.error_h))
Prophet.MAPE.h <- mean(abs(Prophet.error_h)/abs(valid))*100
Prophet.MAE.h #222.732
Prophet.MAPE.h #5.732994


## Neural Network ##################################################################################
# NN.Model <- nnetar(diff(energy, 24), p = 0, P = 10) from HW-2

######################################################################################################
set.seed(12345)
NN.Model <- nnetar(diff(train, 24), p = 3, P = 15)
NN.Forecast <- forecast::forecast(NN.Model, h = 168)

Pass.Forecast <- rep(NA, 168)


# if i <= 24, 1st season 24 hours; if i > 24, use forecast value for prediction because y_actual 
# is not available on the training data
for(i in 1:168){
  if (i <= 24) {
    Pass.Forecast[i] <- train[length(train) - 24 + i] + NN.Forecast$mean[i]
  } else {
    Pass.Forecast[i] <- Pass.Forecast[i-24] + NN.Forecast$mean[i]
  }
}


# Calculate prediction errors from forecast
NN.error <- valid - Pass.Forecast

# Calculate prediction error statistics (MAE and MAPE)
NN.MAE <- mean(abs(NN.error))
NN.MAPE <- mean(abs(NN.error)/abs(valid))*100
NN.MAE #185.7604
NN.MAPE #4.733038



## ensemble model-1 ######################################################################################
########################################################################################################
#average all 4 models
For.Avg <- (hwes.energy.m$mean + 
              forecast::forecast(S.ARIMA, h = 168, xreg = spike2)$mean + 
              tail(fcst_h$yhat, 168) +
              Pass.Forecast)/4
#validation 
Avg.error <- valid - For.Avg
Avg.MAE <- mean(abs(Avg.error))
Avg.MAE #170.0454
Avg.MAPE <- mean(abs(Avg.error)/abs(valid))*100
Avg.MAPE #4.237649


## ensemble model-2 ######################################################################################
########################################################################################################

#remove holt winters b/c highest mape and doens't do well beyond next point
For.Avg2 <- (forecast::forecast(S.ARIMA, h = 168, xreg = spike2)$mean + 
              tail(fcst_h$yhat, 168) +
              Pass.Forecast)/3
#validation 
Avg.error2 <- valid - For.Avg2
Avg.MAE2 <- mean(abs(Avg.error2))
Avg.MAE2 # 148.8195
Avg.MAPE2 <- mean(abs(Avg.error2)/abs(valid))*100
Avg.MAPE2 # 3.739874


########## ensemble 3
##########################################################################
#remove holt winters b/c highest mape and doens't do well beyond next point
For.Avg3 <- (forecast::forecast(S.ARIMA, h = 168, xreg = spike2)$mean + 
               (tail(fcst_h$yhat, 168))/2 +
               (Pass.Forecast)*1.5)/3
#validation 
Avg.error3 <- valid - For.Avg3
Avg.MAE3 <- mean(abs(Avg.error3))
Avg.MAE3 # 146.245
Avg.MAPE3 <- mean(abs(Avg.error3)/abs(valid))*100
Avg.MAPE3 # 3.666625


###############################################################33
########## testing on 10/26, 10/27 - 11/2#############################
###############################################################################

#arima
spike <- rep(0, 33455)
spike[c(26101:26125)] <- 1

S.ARIMA <- Arima(energy, order=c(2,0,2), seasonal=c(3,1,2),xreg = spike, method = "CSS")


### Prophet
holiday_s <- data.frame(
  holiday = 'storm2022',
  ds = as.Date(c('2022-12-23 12:00:00', '2022-12-23 13:00:00',
                 '2022-12-23 14:00:00', '2022-12-23 15:00:00', '2022-12-23 16:00:00',
                 '2022-12-23 17:00:00', '2022-12-23 18:00:00', '2022-12-23 19:00:00',
                 '2022-12-23 20:00:00', '2022-12-23 21:00:00', '2022-12-23 22:00:00', 
                 '2022-12-23 23:00:00', '2022-12-24 00:00:00', '2022-12-24 01:00:00',
                 '2022-12-24 02:00:00', '2022-12-24 03:00:00', '2022-12-24 04:00:00',
                 '2022-12-24 05:00:00', '2022-12-24 06:00:00', '2022-12-24 07:00:00',
                 '2022-12-24 08:00:00', '2022-12-24 09:00:00', '2022-12-24 10:00:00',
                 '2022-12-24 11:00:00', '2022-12-24 12:00:00')),
  lower_window = 0,
  upper_window = 0
)

Prof_h <- prophet(holidays = holiday_s, changepoint.prior.scale = 0.01)
Prof_h <- add_country_holidays(Prof_h, "US")
#Prof_h <- add_seasonality(Prof_h, name='hourly', period=1/24, fourier.order=12)
Prof_h <- fit.prophet(Prof_h, aep_prophet_mod)

# predict
future_h <- make_future_dataframe(Prof_h, periods = 192, freq = 3600) # predict new week - 192 rows
#'day', 'week', 'month', 'quarter', 'year', 1(1 sec), 60(1 minute) or 3600(1 hour).
fcst_h <- predict(Prof_h, future_h)


#### Neural Net
set.seed(12345)
NN.Model <- nnetar(diff(train, 24), p = 3, P = 15)
NN.Forecast <- forecast::forecast(NN.Model, h = 192)

Pass.Forecast <- rep(NA, 192)


# if i <= 24, 1st season 24 hours; if i > 24, use forecast value for prediction because y_actual 
# is not available on the training data
for(i in 1:192){
  if (i <= 24) {
    Pass.Forecast[i] <- train[length(train) - 24 + i] + NN.Forecast$mean[i]
  } else {
    Pass.Forecast[i] <- Pass.Forecast[i-24] + NN.Forecast$mean[i]
  }
}


########## ensemble 3
##########################################################################
#remove holt winters b/c highest mape and doens't do well beyond next point
spike2 <- rep(0,192)
For.Avg3 <- (forecast::forecast(S.ARIMA, h = 192, xreg = spike2)$mean + 
               (tail(fcst_h$yhat, 192))/2 +
               (Pass.Forecast)*1.5)/3


For.Avg3 <- tail(For.Avg3, 168)


x <- write.csv(For.Avg3, "Orange12_Predictions.csv")
