library(tidyverse)
library(tseries)
library(forecast)
library(zoo)
library(lubridate)
library(aTSA)
library(imputeTS)

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

#convert data to time series object
energy <- ts(aep_mod$avgMW, start = 2016, frequency = 12)

autoplot(energy) #seasonality and trend

#We see no missing values in the dataset
ggplot_na_distribution(energy) +  #time series plot that highlights missing values
  labs(y = "Megawatt Usage")

#train/test/valid split
train <- subset(energy, end= length(energy)-7)
valid <- subset(energy, start = length(energy)-6, end = length(energy) -3)
test <- subset(energy, start = length(energy)-2)
train.valid <- subset(energy, end = length(energy) -3)

#plot and decompose (classical)
classical_decomp <- decompose(train, type = "multiplicative")
plot(classical_decomp) #appears no trend


#adjust for seasonality
seas_adj <- train/classical_decomp$seasonal

autoplot(train) + 
  geom_line(aes(y = classical_decomp$trend), color = 'blue') +
  geom_line(aes(y = seas_adj), color = "orange") +
  theme_classic()


#Check stationarity first :: non zero mean, type 2 test
aTSA::adf.test(seas_adj) #fail to reject lags at 0.05 level
#fail to reject null for at least one = random walk!! need to take differences

ggAcf(seas_adj) +
  theme_classic() +
  ggtitle("ACF Plot of Seasonally Adjusted Dataset")

ggPacf(seas_adj) +
  theme_classic() +
  ggtitle("PACF Plot of Seasonally Adjusted Dataset")


################################################################
################## ARIMA Modeling #############################
#############################################################

#Looking for seasonally adjusted, no trend


##################################################################
#################### Auto ARIMA #################################
###############################################################

arima.three <- auto.arima(seas_adj, seasonal = FALSE)
summary(arima.three) #c(0,1,1)

AIC(arima.three)

#white noise check - check normality, constant variance, and independent observations
ggAcf(arima.three$residuals, lag = 12)
ggPacf(arima.three$residuals, lag = 12)


#test of Normality
ggplot(data = seas_adj, aes(x = arima.three$residuals)) + 
  geom_histogram() +
  labs(title = "Histogram of Residuals for Auto ARIMA", x = "Residuals", y = "Frequency")

#formal test of Normality (Ho: Normal, Ha: not normal)
shapiro.test(arima.three$residuals)

#Test of equal variances (Ho: equal variance, Ha: unequal variance)
cor.test(abs(arima.three$residuals),arima.three$fitted,method="spearman",exact=T)


#Checking autocorrelation
pacf1 <- Pacf(seas_adj, lag = 10)$acf
index1=seq(1,length(pacf1))
White.LB <- rep(NA, 10)

####Fit appropriate model
for(i in 3:10){
  White.LB[i] <- Box.test(arima.three$residuals, lag=i, type="Ljung-Box", fitdf = 2)$p.value
}

white.dat=data.frame(cbind(White.LB[3:10],index1[3:10]))
colnames(white.dat)=c("pvalues","Lag") 


#NO WHITE NOISE!! (Ho: no autocorrelation, Ha: autocorrelation exists)
ggplot(white.dat,aes(x=factor(Lag),y=pvalues))+
  geom_col()+
  labs(title="Ljung-Box test",x="Lags",y="p-values")

##################################################################
#################### Trying another model #################################
###############################################################

arima.four <- Arima(seas_adj, order = c(2,1,3))
summary(arima.four) #c(2,1,3)

AIC(arima.four)

#white noise check - check normality, constant variance, and independent observations
ggAcf(arima.four$residuals, lag = 12)
ggPacf(arima.four$residuals, lag = 12)


#test of Normality
ggplot(data = seas_adj, aes(x = arima.four$residuals)) + 
  geom_histogram() +
  labs(title = "Histogram of Residuals for Auto ARIMA", x = "Residuals", y = "Frequency")

#formal test of Normality (Ho: Normal, Ha: not normal)
shapiro.test(arima.four$residuals)

#Test of equal variances (Ho: equal variance, Ha: unequal variance)
cor.test(abs(arima.four$residuals),arima.four$fitted,method="spearman",exact=T)


#Checking autocorrelation
pacf1 <- Pacf(seas_adj, lag = 10)$acf
index1=seq(1,length(pacf1))
White.LB <- rep(NA, 10)

####Fit appropriate model
for(i in 6:10){
  White.LB[i] <- Box.test(arima.four$residuals, lag=i, type="Ljung-Box", fitdf = 5)$p.value
}

white.dat=data.frame(cbind(White.LB[6:10],index1[6:10]))
colnames(white.dat)=c("pvalues","Lag") 


#NO WHITE NOISE!! (Ho: no autocorrelation, Ha: autocorrelation exists)
ggplot(white.dat,aes(x=factor(Lag),y=pvalues))+
  geom_col()+
  labs(title="Ljung-Box test",x="Lags",y="p-values")

###########################################################################
######################Comparing models on validation set############################
############################################################################

AIC(arima.three) #best
AIC(arima.four)


mod3.forecast <- forecast::forecast(arima.three, h = 4) #c(0,1,1)
mod4.forecast <- forecast::forecast(arima.four, h = 4) #c(2,1,3)

########## Fitted vs Actual Graph with Forecast
autoplot(mod3.forecast) +
  autolayer(fitted(arima.three), series = "Fitted") +
  geom_vline(xintercept = 2023, lty = "dashed", color = "red")

autoplot(mod4.forecast) +
  autolayer(fitted(arima.four), series = "Fitted") +
  geom_vline(xintercept = 2023, lty = "dashed", color = "red")

###################### Forecasted vs Actual on Validation Plot

#Here, I multiply by classical_decomp$seasonal[1:4] to look at original 
#   validation data, multiplying back in the seasonal component

#I use [1:4] because the seasonal component is the same within each month

#plots seem to do pretty well!

mod3.forecast$mean <- mod3.forecast$mean * classical_decomp$seasonal[1:4]
mod4.forecast$mean <- mod4.forecast$mean * classical_decomp$seasonal[1:4]

autoplot(valid) + #model 3
  geom_line(aes(y = mod3.forecast$mean), color = "red") + 
  labs(title = "Forecasted vs Actual Time Series Plot of Average Megawatt Usage (2023 Q1)", x = "Months of 2023", y = "Metered Load Megawatt Usage") +
  theme_classic() 
#scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr"))

autoplot(valid) + #model 4
  geom_line(aes(y = mod4.forecast$mean), color = "red") + 
  labs(title = "Forecasted vs Actual Time Series Plot of Average Megawatt Usage (2023 Q1)", x = "Months of 2023", y = "Metered Load Megawatt Usage") +
  theme_classic() 
#scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr"))

########Calculation of MAPE on validation
e3 <- valid - mod3.forecast$mean #c(0,1,1)
MAPE <- mean(abs(e3)/abs(valid))
MAPE #0.06085456

e4 <- valid - mod4.forecast$mean  #c(2,1,3)
MAPE <- mean(abs(e4)/abs(valid))
MAPE #0.05863188

#####################################################################
################# Accuracy on Test Data ############################
#######################################################################

#Use combined train/valid set to predict test and get MAE vals

#Will use 2 best performing models from validation: arima.three & arima.four

cd2 <- decompose(train.valid, type = "multiplicative")

#adjust for seasonality
seas_adj.test <- train.valid/cd2$seasonal

arima.three.test <- Arima(seas_adj.test, order = c(0,1,1))
arima.four.test <- Arima(seas_adj.test, order = c(2,1,3))

mod3.forecast.test <- forecast::forecast(arima.three.test, h = 3)
mod4.forecast.test <- forecast::forecast(arima.four.test, h = 3)

#multiplying back in seasonality component on actual & forecasted pieces
mod3.forecast.test$mean <- mod3.forecast.test$mean * cd2$seasonal[5:7]
mod4.forecast.test$mean <- mod4.forecast.test$mean * cd2$seasonal[5:7]

#testing for MAE
e3.test <- test - mod3.forecast.test$mean #c(0,1,1)
MAPE <- mean(abs(e3.test)/abs(test))
MAPE #0.01733648

e4.test <- test - mod4.forecast.test$mean #c(2,1,3)
MAPE <- mean(abs(e4.test)/abs(test))
MAPE #0.02586696
