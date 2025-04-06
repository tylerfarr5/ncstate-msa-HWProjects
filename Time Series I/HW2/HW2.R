library(tidyverse)
library(tseries)
library(forecast)
library(zoo)
library(lubridate)


aep <- read.csv("https://raw.githubusercontent.com/sjsimmo2/TimeSeries/master/AEP_hourly.csv")

View(aep)
str(aep)

#roll up to month
aep$date2 <- mdy_hm(aep$datetime_beginning_ept)
aep$month <- month(aep$date2)
aep$year <- year(aep$date2)

aep_mod <- aep %>%
  group_by(month, year) %>%
  summarise(avgMW = mean(mw)) %>%
  arrange(year, month)

#train/test/valid split
train <- aep_mod %>%
  filter(year %in% c(2016,2017, 2018,2019,2020,2021,2022))

validation <- aep_mod %>%
  filter(year == 2023 & month %in% c(1,2,3,4))

test <- aep_mod %>%
  filter(year == 2023 & month %in% c(5,6,7))

train_validation <- aep_mod %>%
  filter((year %in% c(2016,2017, 2018,2019,2020,2021,2022)) | (year == 2023 & month %in% c(1,2,3,4)))

##############################################################
################ Question1 ##################################
#############################################################


energy <- ts(train$avgMW, start = 2016, frequency = 12)

autoplot(energy)

decomp_stl <- stl(energy, s.window = 15)
plot(decomp_stl) #use STL decomposition instead of classical or X13. Allows for changing effects of trend + season

autoplot(energy) + 
  geom_line(aes(y = decomp_stl$time.series[,2]), color = 'blue') +
  labs(x = "Year (2016 - 2022)", y = "Metered Load Megawatts Usage", title = "Time Series Plot of Average Megawatt Usage, STL Decomposition (2016 - 2022)") +
  theme_classic()


Ft=max(0,1-var(decomp_stl$time.series[,3])/(var(decomp_stl$time.series[,3])+var(decomp_stl$time.series[,2]) + 2*cov(decomp_stl$time.series[,3],decomp_stl$time.series[,2])))
Ft #0.35, weak trend
Fs=max(0,1-var(decomp_stl$time.series[,3])/(var(decomp_stl$time.series[,3])+var(decomp_stl$time.series[,1])+2*cov(decomp_stl$time.series[,3],decomp_stl$time.series[,1])))
Fs #0.88, strong trend

##########validation plot
hwes.energy.a <- hw(energy, seasonal = "additive", initial = "optimal", h = 4)
summary(hwes.energy.a)
hwes.energy.m <- hw(energy, seasonal = "multiplicative", initial = "optimal", h = 4)
summary(hwes.energy.m) #multiplicative has lower AIC


hwes.energy.val <- hw(energy.v, seasonal = "multiplicative", inital = "optimal", h = 4)
energy.v <- ts(validation$avgMW, start = 1)

autoplot(energy.v) + 
  geom_line(aes(y = hwes.energy.m$mean), color = "red") + 
  labs(title = "Predicted vs Actual Time Series Plot of Average Megawatt Usage (2023 Q1)", x = "Months of 2023", y = "Metered Load Megawatt Usage") +
  theme_classic() +
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr")) +
  #scale_color_manual(name = "Line", breaks = c("Actual", "Predicted"), values = c('Actual'='black','Predicted'='red'))

#can't get actual validation plot values. Also is there better predict than this?
#utoplot(hwes.energy.m)+
#  autolayer(fitted(hwes.energy.m),series="Fitted")+
#  ylab("Megawatts Usage")+ 
#  geom_vline(xintercept = 2023,color="red",linetype="dashed")

##############################################################
################ Question 2 ##################################
#############################################################


#### Model 1
e1 <- validation$avgMW - hwes.energy.a$mean #additive
e2 <- validation$avgMW - hwes.energy.m$mean #multiplicative

MAPE <- mean(abs(e1)/abs(validation$avgMW))
MAPE

MAPE <- mean(abs(e2)/abs(validation$avgMW))
MAPE

#### Model 2
#ets model
ets.energy <- ets(energy)
summary(ets.energy)

ets.forecast.energy <- ets.energy %>%
  forecast::forecast(h = 4)

error <- validation$avgMW - ets.forecast.energy$mean

MAPE <- mean(abs(error)/abs(validation$avgMW))
MAPE

##############Using test data
energy.trv <- ts(train_validation$avgMW, start = 2016, frequency = 12)
hwes.energy.m <- hw(energy.trv, seasonal = "multiplicative", initial = "optimal", h = 3)
summary(hwes.energy.m)

error <- test$avgMW - hwes.energy.m$mean

MAPE <- mean(abs(error)/abs(test$avgMW))
MAPE
