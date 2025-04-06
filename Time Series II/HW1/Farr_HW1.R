library(ggplot2)
library(tidyverse)
library(dplyr)
library(tseries)
library(forecast)
library(zoo)
library(lubridate)
library(aTSA)
library(imputeTS)
##########################################################################
#################### Reading in the data ################################
################################################################################

# combining any validation sets with train to train on more recent data?
#start point for 1/1/2019 and 1/1/2020-->removed 2019 not relevant

aep_train <- read.csv("hrl_load_metered.csv")
aep_valid <- read.csv("hrl_load_metered - test1.csv") #9/22 - 9/27
aep_valid2 <- read.csv("hrl_load_metered - test2.csv") #9/28 - 10/4

aep <- rbind(aep_train, aep_valid, aep_valid2)

View(aep)
str(aep)

#converting to date/time object & pulling month and year columns out for TS
aep$date2 <- mdy_hm(aep$datetime_beginning_ept)
aep$month <- month(aep$date2)
aep$year <- year(aep$date2)
aep$hour <- hour(aep$date2)
aep$day <- day(aep$date2)

aep_mod <- aep %>%
  select(hour,day, month, year, mw) %>%
  arrange(year, month,day,hour)

#start data on 1/1/2020 - index 35605
#1/1/2019 = index 26305
aep_mod <- aep_mod[35065:nrow(aep_mod),]

#converting to time series object
energy <- ts(aep_mod$mw, start = c(2020,1), frequency = 24)

autoplot(energy)
ggsubseriesplot(energy)

#time series plot that highlights missing values - we see NONE
ggplot_na_distribution(energy) +
  labs(y = "Megawatt Usage")

########################################################################
######################### train/test/valid split ########################
#######################################################################

#train = original, valid = week 1 only
#train <- subset(energy, end= length(energy)-312) 
#valid <- subset(energy, start = length(energy)-311, end = length(energy) - 168)

#train = original + week 1, valid = week 2 only
train <- subset(energy, end= length(energy)-168) #includes original + week 1
valid <- subset(energy, start = length(energy)-167) #week 2 data


#######################################################################
######################## Decomposition ##################################
#######################################################################

decomp_stl <- stl(train, s.window = 7)
plot(decomp_stl)

autoplot(train) +
  theme_classic() +
  ggtitle("Hourly Energy Usage Over Time (1/1/2020 - 9/22/2023)") +
  xlab("Time") +
  ylab("Energy Usage (megawatts)")

aTSA::adf.test(train) #looked at type 2. don't see a trend & timeplot fluctuates around a nonzero value
#reject all lags so it is stationary about the nonzero mean

############################################################
############# ETS - takes a longggg time to run ###########
###############################################################3
ets.energy <- ets(train) #can't run with frequency > 24, this takes a long time to run
summary(ets.energy) #ETS(A,Ad,N) 

ets.forecast.energy <- ets.energy %>%
  forecast::forecast(h = 144)

error <- valid - ets.forecast.energy$mean

MAPE <- mean(abs(error)/abs(valid))
MAPE

########################################################
################# Holt Winters ######################
############################################################

#make h = 144 if forecasting week 1, h = 168 if week 2
hwes.energy.m <- hw(train, seasonal = "multiplicative", initial = "optimal", h = 168)
summary(hwes.energy.m)

error <- valid - hwes.energy.m$mean

MAPE <- mean(abs(error)/abs(valid))
MAPE 
#2020 = 0.0664

MAE <- mean(abs(error))
MAE 
#2020 = 269.21


autoplot(hwes.energy.m) +
#  autolayer(fitted(hwes.energy.m), series = "fitted") +
  ylab("Energy Usage")
 # geom_vline(xintercept = 2007.25,color="orange",linetype="dashed")


autoplot(valid) + 
  geom_line(aes(y = hwes.energy.m$mean), color = "red") + 
  labs(title = "Predicted vs Actual Time Series Plot of Average Megawatt Usage (2023 Q1)", x = "Months of 2023", y = "Metered Load Megawatt Usage") +
  theme_classic() +
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr")) 
############################################################################
######################### ARIMA Model ####################################
###########################################################################

#we know we have a season - so should we go deterministic or stochastic?
train %>%
  nsdiffs() #prints a 1, so we are stochastic - seasonal differencing!

train %>%
  diff(lag = 24) %>%
  ndiffs() #no need for regular differencing

#Graph showing a comparison of differenced data to original -looks more stationary!
cbind("Energy Usage" = train,
      "Annual change in Energy Usage" = diff(train, 24)) %>%
  autoplot(facets=TRUE) +
  xlab("Time") + ylab("") +
  ggtitle("Comparison of Differenced Data to Original")

#visualize seasonal difference + ACF/PACF plots
train %>%
  diff(lag= 24) %>%
  ggtsdisplay() #shows seasonal AR lags with exp decreasing ACF 


#################################
######### Seasonal ARIMA: 2020 #########
#################################

# Seasonal ARIMA - trying out our guess at lags from ACF/PACF and nsdiffs() above
train %>% 
  Arima(order=c(2,0,2), seasonal=c(3,1,2), method = 'CSS') %>%
  residuals() %>% 
  ggtsdisplay() #alternative to checkresiduals() function

#creating intervention variable 12/24/2022, (12/23 12 pm - 12/24 12 pm)
############large spikes of megawatt usage (BLACKOUTS)
autoplot(train)
View(data.frame(train)) #viewed df, then sorted by largest values to see index of the outlier

#make spike length of train - 32639 or 32783, depends how we define train above
spike <- rep(0, 32783)
spike[c(26100:26124)] <- 1

#create another intervention var for summer/winter(January/July)? COVID? ...?

#Train = original, Valid = week 1 only
#(3,0,3)(3,1,2)[24] MAE = 251.11, MAPE = 6.80%
#(2,0,2)(3,1,2)[24] MAE = 251.56, MAPE = 6.81%  **
#(2,0,2)(3,1,3)[24] MAE = 256.05, MAPE = 6.93%
#(4,0,4)(3,1,0)[24] MAE = 258.58, MAPE = 7.03%
#(3,0,0)(2,1,0)[24] MAE = 275.64, MAPE = 7.48% (Auto Arima)

#Train = original + week 1, valid = week 2 only
#(2,0,2)(3,1,3)[24] MAE = 162.27, MAPE = 4.03%
#(2,0,2)(3,1,2)[24] MAE = 163.73, MAPE = 4.06% **
#(3,0,3)(3,1,2)[24] MAE = 163.96, MAPE = 4.06%
#(3,0,0)(2,1,0)[24] MAE = 173.75, MAPE = 4.34% (Auto Arima)
#(4,0,4)(3,1,0)[24] MAE = 179.06, MAPE = 4.41%

S.ARIMA <- Arima(train, order=c(2,0,2), seasonal=c(3,1,2),xreg = spike, method = "CSS")
summary(S.ARIMA) 
checkresiduals(S.ARIMA) #shows histogram, Ljung Box test, etc

S.ARIMA %>%
  residuals() %>%
  ggtsdisplay()

#Use auto.arima as starting point. Seasonal = TRUE to allow seasonal differences to be taken

######### Auto Arima does not load fast enough with method = "ML"
S.ARIMA.auto <- auto.arima(train, method="CSS", xreg = spike, seasonal = TRUE)
summary(S.ARIMA.auto) 
checkresiduals(S.ARIMA.auto) #does not completely model white noise


#evaluating model - update all h to be 144 or 168 depending on valid length

#make this spike the length of the validation set
spike2 <- rep(0,168) #create intervention variable for forecasted values

autoplot(forecast::forecast(S.ARIMA, h = 168,xreg = spike2)) + 
  #autolayer(fitted(S.ARIMA), series="Fitted") + 
  ylab("Energy Usage") 
#geom_vline(xintercept = 2007.25,color="orange",linetype="dashed")

S.ARIMA.error <- valid - forecast::forecast(S.ARIMA, h = 168, xreg = spike2)$mean

# Calculate prediction error statistics (MAE and MAPE)
S.ARIMA.MAE <- mean(abs(S.ARIMA.error))
S.ARIMA.MAPE <- mean(abs(S.ARIMA.error)/abs(valid))*100

S.ARIMA.MAE
S.ARIMA.MAPE



HWES.train.m <- hw(ts_obj_21, seasonal = "multiplicative", initial = 'optimal', h=312)
summary(HWES.train.m) # AIC 461397, BIC 461631, MAE 80.46 MAPE 1.84
error_hw_m <- validation$mw - HWES.train.m[["mean"]]
MAE <- mean(abs(error_hw_m))
MAPE <- mean(abs(error_hw_m)/abs(validation$mw))
MAE # 215.4213
MAPE # 0.05678966
#create a dataframe for time plot
# HWES.train.m[["mean"]] - prediction on validation 
df_hw_m <- data.frame(year = validation$year,
                      month = validation$month,
                      day = validation$day,
                      hour = validation$hour,
                      index = seq(1,312),
                      original = validation$mw,
                      prediction = HWES.train.m[["mean"]])

ggplot2::ggplot(data = aep_mod) +
  geom_smooth(aes(x = index, y = prediction, color = 'prediction')) +
  geom_smooth(aes(x = index, y = original, color = 'original')) +
  labs(y = 'mw', x = 'day', title = 'Predicted vs Actual Values for the Validation Data (HW_M)') +
  theme(plot.title = element_text(hjust = 0.5)) +  # center title in the middle
  scale_color_manual(
    name = 'Legend', #Legend title
    values = c('prediction' = 'blue', 'original' = 'black'),
    labels = c('original','prediction')
  )
