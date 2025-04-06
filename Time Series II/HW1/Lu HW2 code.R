library(ggplot2)
library(tidyverse)
library(dplyr)
library(tseries)
library(forecast)
library(zoo)
library(lubridate)
library(aTSA)
library(imputeTS)
#install.packages('prophet')
library(prophet)
#################### Read data ################################

# combining any validation sets with train to train on more recent data?
# start point for 1/1/2019 and 1/1/2020-->removed 2019 not relevant

aep_train <- read.csv("G:/My Drive/MSA/Class Material/Fall 2 Time Series II/HW/Homework1_TS2/hrl_load_metered.csv")
aep_valid <- read.csv("G:/My Drive/MSA/Class Material/Fall 2 Time Series II/HW/Homework1_TS2/hrl_load_metered - test1.csv") #9/22 - 9/27
aep_valid2 <- read.csv("G:/My Drive/MSA/Class Material/Fall 2 Time Series II/HW/Homework1_TS2/hrl_load_metered - test2.csv") #9/28 - 10/4
aep_valid3 <- read.csv("G:/My Drive/MSA/Class Material/Fall 2 Time Series II/HW/Homework2_TS2/hrl_load_metered - test3.csv") 
aep_valid4 <- read.csv("G:/My Drive/MSA/Class Material/Fall 2 Time Series II/HW/Homework2_TS2/hrl_load_metered - test4.csv") 

# training data for 3rd week
aep <- rbind(aep_train, aep_valid, aep_valid2)
aep <- aep[,c(1,6)]

View(aep)
str(aep)

## create a new df that follow prophet requirement### 
##### training data for 3rd week####
aep_prophet_mod <- aep %>%
  mutate(datetime_beginning_ept = strptime(datetime_beginning_ept, format = "%m/%d/%y %H:%M")) %>%
  mutate(datetime_beginning_ept = format(datetime_beginning_ept, format = '%Y-%m-%d %H:%M:%S')) %>%
dplyr::select(datetime_beginning_ept, mw)

colnames(aep_prophet_mod)[1] <- 'ds'
colnames(aep_prophet_mod)[2] <- 'y'

aep_prophet <- data.frame(ds = aep_prophet_mod$ds, y = aep_prophet_mod$y)

#start data on 1/1/2020 - index 35065
#1/1/2019 = index 26305
aep_prophet <- aep_prophet[35065:nrow(aep_prophet),]
#aep <- aep[35065:nrow(aep),]
#aep$new_index <- 1:nrow(aep)

##### training data for 4th week####
aep_val3 <- aep_valid3[ ,c(2,7)]
# convert timestamp for validation 3
aep_val3 <- aep_val3 %>%
  mutate(datetime_beginning_ept = strptime(datetime_beginning_ept, format = "%m/%d/%Y %I:%M:%S %p")) %>%
  mutate(datetime_beginning_ept = format(datetime_beginning_ept, format = '%Y-%m-%d %H:%M:%S')) %>%
  dplyr::select(datetime_beginning_ept, mw)

colnames(aep_val3)[1] <- 'ds'
colnames(aep_val3)[2] <- 'y'

aep_2 <- rbind(aep_prophet, aep_val3)
aep_prophet_2 <- data.frame(ds = aep_2$ds, y = aep_2$y)


#prophet model w/o holiday (point intervention)####
# dataframe - aep_prophet

#build prophet model
###  validation on week 3####
Prof <- prophet(changepoint.prior.scale = 0.01)
Prof <- add_country_holidays(Prof, "US")
#Prof <- add_seasonality(Prof, name='hourly', period= 24, fourier.order=12) # you don't need to set 
#up seasonality for sub-daily data. the algorithm already takes care of it.If you add seasonality, the model will overdo it.
Prof <- fit.prophet(Prof, aep_prophet)

# predict
future <- make_future_dataframe(Prof, periods = 168, freq = 3600) # predict new week - 168 row
#'day', 'week', 'month', 'quarter', 'year', 1(1 sec), 60(1 minute) or 3600(1 hour).
fcst <- predict(Prof, future)
#plot(Prof, fcst)

# MAPE
# Calculate prediction errors from forecast
Prophet.error <- aep_valid3$mw - tail(predict(Prof, future)$yhat, 168)
# Calculate prediction error statistics (MAE and MAPE)
Prophet.MAE <- mean(abs(Prophet.error))
Prophet.MAPE <- mean(abs(Prophet.error)/abs(aep_valid3$mw))*100
Prophet.MAE #176.6085
Prophet.MAPE #4.689731
#plot the vailidation 3
# plots of predicted vs actual for Prophet
prophet_val3_table <- data.frame(Time = 1:168, Actual = aep_valid3$mw, Predicted = fcst$yhat[32949:33116])
library(ggplot2)
# plot the predicted vs actual 
ggplot(prophet_val3_table, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(x = 'Day', y = 'Megawatts') +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "blue")) +
  scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144,168), 
                     labels = c('10/5/23', '10/6/23', '10/7/23', '10/8/23', '10/9/23', '10/10/23', '10/11/23', '10/12/23')) +
  theme(axis.text.x = element_text(angle = 45)) 
  #theme_classic() 
  #theme_bw() +
  #theme_minimal() 
 
 
### validation on week 4####
Prof_2 <- prophet(changepoint.prior.scale = 0.01)
Prof_2 <- add_country_holidays(Prof_2, "US")
#Prof <- add_seasonality(Prof, name='hourly', period= 24, fourier.order=12) # you don't need to set 
#up seasonality for sub-daily data. the algorithm already takes care of it.If you add seasonality, the model will overdo it.
Prof_2 <- fit.prophet(Prof_2, aep_prophet_2)

# predict
future_2 <- make_future_dataframe(Prof_2, periods = 168, freq = 3600) # predict new week - 168 row
#'day', 'week', 'month', 'quarter', 'year', 1(1 sec), 60(1 minute) or 3600(1 hour).
fcst_2 <- predict(Prof_2, future_2)
#plot(Prof_2, fcst_2)


# MAPE
# Calculate prediction errors from forecast
Prophet.error.2 <- aep_valid4$mw - tail(predict(Prof_2, future_2)$yhat, 168)
# Calculate prediction error statistics (MAE and MAPE)
Prophet.MAE.2 <- mean(abs(Prophet.error.2))
Prophet.MAPE.2 <- mean(abs(Prophet.error.2)/abs(aep_valid4$mw))*100
Prophet.MAE.2 #135.6816
Prophet.MAPE.2 #3.588571

#plot the vailidation 4
# plots of predicted vs actual for Prophet
prophet_val4_table <- data.frame(Time = 1:168, Actual = aep_valid4$mw, Predicted = fcst_2$yhat[33117:33284])
library(ggplot2)
# plot the predicted vs actual 
ggplot(prophet_val4_table, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(x = 'Day', y = 'Megawatts') +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "blue")) +
  scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144,168), 
                     labels = c('10/12/23', '10/13/23', '10/14/23', '10/15/23', '10/16/23', '10/17/23', '10/18/23', '10/19/23')) +
  theme(axis.text.x = element_text(angle = 45)) 

  #theme_bw() +
  #theme_minimal()

#prophet model w/ holiday (point intervention)####

# create holiday for point intervention
#creating intervention variable 12/24/2022, (12/23 12 pm - 12/24 12 pm), large spikes of megawatt usage (BLACKOUTS)
#autoplot(train)
#View(data.frame(train)) #viewed df, then sorted by largest values to see index of the outlier
#make spike length of train - 32639 or 32783, depends how we define train above
#spike <- rep(0, 32951)
#spike[c(26100:26124)] <- 1
holiday_s <- data.frame(
  holiday = 'storm2022',
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


#build prophet model
### validation on week 3####
Prof_h <- prophet(holidays = holiday_s, changepoint.prior.scale = 0.01)
Prof_h <- add_country_holidays(Prof_h, "US")
#Prof_h <- add_seasonality(Prof_h, name='hourly', period=1/24, fourier.order=12)
Prof_h <- fit.prophet(Prof_h, aep_prophet)

# predict
future_h <- make_future_dataframe(Prof_h, periods = 168, freq = 3600) # predict new week - 168 row
#'day', 'week', 'month', 'quarter', 'year', 1(1 sec), 60(1 minute) or 3600(1 hour).
fcst_h <- predict(Prof_h, future_h)
#plot(Prof_h, fcst_h)

# MAPE
# Calculate prediction errors from forecast
Prophet.error_h <- aep_valid3$mw - tail(predict(Prof_h, future_h)$yhat, 168)
# Calculate prediction error statistics (MAE and MAPE)
Prophet.MAE.h <- mean(abs(Prophet.error_h))
Prophet.MAPE.h <- mean(abs(Prophet.error_h)/abs(aep_valid3$mw))*100
Prophet.MAE.h #174.8576
Prophet.MAPE.h #4.640696

#plot the vailidation 3
# plots of predicted vs actual for Prophet
prophet_val3_table_intervention <- data.frame(Time = 1:168, Actual = aep_valid3$mw, Predicted = fcst_h$yhat[32949:33116])

prophet_val3_tablaue <- data.frame(Time = aep_valid3$datetime_beginning_ept, Actual = aep_valid3$mw, Predicted = fcst_h$yhat[32949:33116])
# Write filtered data into a new file.
write.csv(prophet_val3_tablaue, "G:/My Drive/MSA/Class Material/Fall 2 Time Series II/HW/Homework2_TS2/prophet_val3.csv")

library(ggplot2)
# plot the predicted vs actual 
ggplot(prophet_val3_table_intervention, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(x = 'Day', y = 'Megawatts', title = 'Predicted vs. Actual Energy Usage - Prophet Model (10/5/2023-10/11/2023)') +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "orange")) +
  scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144,168), 
                     labels = c('10/5/23', '10/6/23', '10/7/23', '10/8/23', '10/9/23', '10/10/23', '10/11/23', '10/12/23')) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme_classic() 

  #theme_bw() +
  #theme_minimal() 

### validation on week 4####
Prof_h_2 <- prophet(holidays = holiday_s, changepoint.prior.scale = 0.01)
Prof_h_2 <- add_country_holidays(Prof_h_2, "US")
#Prof_h <- add_seasonality(Prof_h, name='hourly', period=1/24, fourier.order=12)
Prof_h_2 <- fit.prophet(Prof_h_2, aep_prophet_2)

# predict
future_h_2 <- make_future_dataframe(Prof_h_2, periods = 168, freq = 3600) # predict new week - 168 row
#'day', 'week', 'month', 'quarter', 'year', 1(1 sec), 60(1 minute) or 3600(1 hour).
fcst_h_2 <- predict(Prof_h_2, future_h_2)
#plot(Prof_h_2, fcst_h_2)

# MAPE
# Calculate prediction errors from forecast
Prophet.error_h.2 <- aep_valid4$mw - tail(predict(Prof_h_2, future_h_2)$yhat, 168)
# Calculate prediction error statistics (MAE and MAPE)
Prophet.MAE.h.2 <- mean(abs(Prophet.error_h.2))
Prophet.MAPE.h.2 <- mean(abs(Prophet.error_h.2)/abs(aep_valid4$mw))*100
Prophet.MAE.h.2 #131.0457
Prophet.MAPE.h.2 #3.466522

#plot the vailidation 4
# plots of predicted vs actual for Prophet
prophet_val4_table_intervention <- data.frame(Time = 1:168, Actual = aep_valid4$mw, Predicted = fcst_h_2$yhat[33117:33284])

prophet_val4_tablaue <- data.frame(Time = aep_valid4$datetime_beginning_ept, Actual = aep_valid4$mw, Predicted = fcst_h_2$yhat[33117:33284])
# Write filtered data into a new file.
write.csv(prophet_val4_tablaue, "G:/My Drive/MSA/Class Material/Fall 2 Time Series II/HW/Homework2_TS2/prophet_val4.csv")


library(ggplot2)
# plot the predicted vs actual 
ggplot(prophet_val4_table_intervention, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(x = 'Day', y = 'Megawatts', title = 'Predicted vs. Actual Energy Usage - Prophet Model (10/12/2023-10/18/2023)') +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "blue")) +
  scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144,168), 
                     labels = c('10/12/23', '10/13/23', '10/14/23', '10/15/23', '10/16/23', '10/17/23', '10/18/23', '10/19/23')) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme_classic() 
  #theme_bw() +
  #theme_minimal()

#neural network ####

# from homework 1 - seasonality data diff=24, no regular diff
#ARIMA(2,0,2)(3,1,2)[24] from homework 1 after taking a seasonal difference of 24
# NN.Model <- nnetar(diff(energy, 24), p = 2, P = 3)
# week 3 validation
# NN.MAE #341.7508
# NN.MAPE #9.173736
# week 4 validation
# NN.MAE.2 #226.5264
# NN.MAPE.2 #6.066252

# NN.Model <- nnetar(diff(energy, 24), p = 2, P = 5)
# week 3 validation
# NN.MAE #339.3257
# NN.MAPE #9.073439
# week 4 validation
# NN.MAE # 170.9458
# NN.MAPE # 4.642233

# NN.Model <- nnetar(diff(energy, 24), p = 0, P = 5) 
# week 3 validation
# NN.MAE #352.5487
# NN.MAPE #9.451573
# week 4 validation
# NN.MAE # 143.2946
# NN.MAPE # 3.907599

# NN.Model <- nnetar(diff(energy, 24), p = 0, P = 7)
# week 3 validation
# NN.MAE #325.7672
# NN.MAPE #8.732222
# week 4 validation
# NN.MAE # 128.5717
# NN.MAPE # 3.515719

# NN.Model <- nnetar(diff(energy, 24), p = 0, P = 24)
# week 3 validation
# NN.MAE #228.5557
# NN.MAPE #6.134434
# week 4 validation
# NN.MAE # 147.5133
# NN.MAPE # 3.909091

# NN.Model <- nnetar(diff(energy, 24), p = 0, P = 10)
# week 3 validation
# NN.MAE #290.7848
# NN.MAPE #7.797715
# week 4 validation
# NN.MAE # 127.9851
# NN.MAPE # 3.479618


### validation on week 3####
energy <- ts(aep_prophet$y,  frequency = 24)
#time series plot that highlights missing values - we see NONE
ggplot_na_distribution(energy) +
  labs(y = "Megawatt Usage")
# check how to make the data stationary 
energy %>%
  nsdiffs() #prints a 1, so we are stochastic - seasonal differencing

energy %>%
  diff(lag = 24) %>%
  ndiffs() #no need for regular differencing

# ARIMA(2,0,2)(3,1,2)[24] from homework 1 after taking a seasonal difference of 24
set.seed(12345)
NN.Model <- nnetar(diff(energy, 24), p = 0, P = 10)
NN.Forecast <- forecast::forecast(NN.Model, h = 168)

Pass.Forecast <- rep(0, 168)

# if i <= 24, 1st season 24 hours; if i > 24, use forecast value for prediction because y_actual 
# is not available on the training data
for(i in 1:168){
  if (i <= 24) {
    Pass.Forecast[i] <- energy[length(energy) - 24 + i] + forecast::forecast(NN.Model, h = 24)$mean[i]
  } else {
    Pass.Forecast[i] <- Pass.Forecast[i-24] + NN.Forecast$mean[i]
  }
}

Pass.Forecast <- ts(Pass.Forecast, frequency = 24)
#valid3 <- ts(aep_valid3$mw,  frequency = 24)

# Calculate prediction errors from forecast
NN.error <- aep_valid3$mw- Pass.Forecast

# Calculate prediction error statistics (MAE and MAPE)
NN.MAE <- mean(abs(NN.error))
NN.MAPE <- mean(abs(NN.error)/abs(aep_valid3$mw))*100
NN.MAE #341.7508
NN.MAPE #9.173736

#plot the validation 3
# plots of predicted vs actual for neural network
prophet_val3_table_nn <- data.frame(Time = 1:168, Actual = aep_valid3$mw, Predicted = Pass.Forecast)

nn_val3_tablaue <- data.frame(Time = aep_valid3$datetime_beginning_ept, Actual = aep_valid3$mw, Predicted = Pass.Forecast)
# Write filtered data into a new file.
write.csv(nn_val3_tablaue, "G:/My Drive/MSA/Class Material/Fall 2 Time Series II/HW/Homework2_TS2/nn_val3_tablaue.csv")

# plot the predicted vs actual 
ggplot(prophet_val3_table_nn, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(x = 'Day', y = 'Megawatts', title = 'Predicted vs. Actual Energy Usage - Neural Network Model (10/5/2023-10/11/2023)') +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "orange")) +
  scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144,168), 
                     labels = c('10/5/23', '10/6/23', '10/7/23', '10/8/23', '10/9/23', '10/10/23', '10/11/23', '10/12/23')) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme_classic() 
  

### validation on week 4####
energy.2 <- ts(aep_prophet_2$y,  frequency = 24)
# ARIMA(2,0,2)(3,1,2)[24] from homework 1 after taking a seasonal difference of 24
set.seed(12345)
NN.Model.2 <- nnetar(diff(energy.2, 24), p = 0, P = 10)
NN.Forecast.2 <- forecast::forecast(NN.Model.2, h = 168)

Pass.Forecast.2 <- rep(0, 168)

# if i <= 24, 1st season 24 hours; if i > 24, use forecast value for prediction because y_actual is not available on the training data
for(i in 1:168){
  if (i <= 24) {
    Pass.Forecast.2[i] <- energy.2[length(energy.2) - 24 + i] + NN.Forecast.2$mean[i]
  } else {
    Pass.Forecast.2[i] <- Pass.Forecast.2[i-24] + NN.Forecast.2$mean[i]
  }
  
}

# Calculate prediction error statistics (MAE and MAPE)
NN.error.2 <- aep_valid4$mw - Pass.Forecast.2
NN.MAE.2 <- mean(abs(NN.error.2))
NN.MAPE.2 <- mean(abs(NN.error.2)/abs(aep_valid4$mw))*100
NN.MAE.2 #226.5264
NN.MAPE.2 #6.066252

#plot the validation 4
# plots of predicted vs actual for neural network
prophet_val4_table_nn <- data.frame(Time = 1:168, Actual = aep_valid4$mw, Predicted = Pass.Forecast.2)

nn_val4_tablaue <- data.frame(Time = aep_valid4$datetime_beginning_ept, Actual = aep_valid4$mw, Predicted = Pass.Forecast.2)
# Write filtered data into a new file.
write.csv(nn_val4_tablaue, "G:/My Drive/MSA/Class Material/Fall 2 Time Series II/HW/Homework2_TS2/nn_val4_tablaue.csv")


# plot the predicted vs actual 
ggplot(prophet_val4_table_nn, aes(x = Time)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(x = 'Day', y = 'Megawatts', title = 'Predicted vs. Actual Energy Usage - Neural Network Model (10/12/2023-10/18/2023)') +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "blue")) +
  scale_x_continuous(breaks = c(1, 24, 48, 72, 96, 120, 144,168), 
                     labels = c('10/12/23', '10/13/23', '10/14/23', '10/15/23', '10/16/23', '10/17/23', '10/18/23', '10/19/23')) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme_classic() 
  #theme_bw() +
  #theme_minimal()
