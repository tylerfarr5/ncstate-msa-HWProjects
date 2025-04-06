# -*- coding: utf-8 -*-
"""
Created on Mon Sep 25 19:10:00 2023

@author: thfar
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import pyplot
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.tsa.seasonal import STL
from datetime import datetime
from statsmodels.tsa.api import ExponentialSmoothing, SimpleExpSmoothing, Holt
import warnings
from statsmodels.tsa.stattools import adfuller
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.graphics.tsaplots import plot_pacf
from statsmodels.tsa.arima.model import ARIMA
import statsmodels.api as sm
import pmdarima as pm


###########################################################################
#################### Reading in Time Series Data ##########################
########################################################################

aep = pd.read_csv("https://raw.githubusercontent.com/sjsimmo2/TimeSeries/master/AEP_hourly.csv")

aep['datetime_beginning_ept'] = pd.to_datetime(aep['datetime_beginning_ept'])
aep['month'] = aep['datetime_beginning_ept'].dt.month
aep['year'] = aep['datetime_beginning_ept'].dt.year

aep_mod = aep.groupby(['month', 'year']).mean('mw').sort_values(['year', 'month'])
aep_mod.reset_index()

df = pd.date_range(start = "1/1/2016", end= "7/31/2023", freq = "MS")
aep_mod.index = pd.to_datetime(df)


#explore
print(aep_mod.head())

plt.plot(aep_mod['mw'])
plt.xlabel("Time")
plt.ylabel("Average Metered Load Megawatt Usage")
plt.title("Average Megawatt Usage Over Time")
plt.show()

###########################################################################
######################## Decomposition ######################################
########################################################################

#Classical Decomposiion

result = seasonal_decompose(aep_mod['mw'], model = "multiplicative", period = 12)
result.plot()
pyplot.show()


#STL Decomposition

stl = STL(aep_mod['mw'], period = 12)
res = stl.fit()
fig = res.plot()
pyplot.show()


#######################################################################
############### Exponential Smoothing ##############################
####################################################################

######Simple
fit1 = SimpleExpSmoothing(aep_mod['mw']).fit()
fit1.params['smoothing_level']

fcast1 = fit1.forecast(24)

plt.plot(aep_mod['mw'],color = "black")
plt.plot(fcast1, color = "red")
plt.show()

##########Holt
fit2 = Holt(aep_mod['mw']).fit() #additive
fit2.params['smoothing_level']
fit2.summary()

fcast2 = fit2.forecast(24)

plt.plot(aep_mod['mw'], color = "black")
plt.plot(fcast2, color = "red")
plt.show()

#----------------------------------------------------------------------
warnings.filterwarnings('ignore')
fit3 = Holt(aep_mod['mw'], exponential= True).fit() #multiplicative
fit3.summary()

fcast3 = fit3.forecast(24)

plt.plot(aep_mod['mw'], color = "black")
plt.plot(fcast3, color = "red")
plt.show()


#----------------------------------------------------------------------
fit4 = Holt(aep_mod['mw'], damped = True).fit() #damped trend w additive
fit4.summary()

fcast4 = fit4.forecast(24)

plt.plot(aep_mod['mw'], color = "black")
plt.plot(fcast4, color = "red")
plt.show()

#----------------------------------------------------------------------
fit5 = Holt(aep_mod['mw'], damped = True, exponential = True).fit() #damped trend w multiplicative
fit5.summary()

fcast5 = fit5.forecast(24)

plt.plot(aep_mod['mw'], color = "black")
plt.plot(fcast5, color = "red")
plt.show()

#shows all forecasts
ax = aep_mod['mw'].plot(color = "black", figsize = (12,8))
fcast1.plot(ax = ax, color = "blue")
fcast2.plot(ax = ax, color = "orange")
fcast3.plot(ax = ax, color = "red")
fcast4.plot(ax = ax, color = "green")
fcast5.plot(ax = ax, color = "purple")
plt.show()


########### Holt Winters
#additive
aep_mod['HWES_ADD'] = ExponentialSmoothing(aep_mod['mw'], trend = 'add', seasonal = 'add', seasonal_periods = 12).fit().fittedvalues
#multiplicative
aep_mod['HWES_MUL'] = ExponentialSmoothing(aep_mod['mw'], trend = 'mul', seasonal = 'mul', seasonal_periods = 12).fit().fittedvalues


aep_mod[['mw', 'HWES_ADD', 'HWES_MUL']].plot(title='Holt Winters Exponential Smoothing: Additive and Multiplicative Seasonality')


fit6 = ExponentialSmoothing(aep_mod['mw'], trend = 'add', seasonal = 'mul', seasonal_periods = 12).fit()
fit6.summary()

fit7 = ExponentialSmoothing(aep_mod['mw'], trend = 'add', seasonal = 'add', seasonal_periods = 12).fit()
fit7.summary()

fcast6 = fit6.forecast(24)
fcast7 = fit7.forecast(24)

ax2 = aep_mod['mw'].plot(color = 'black', figsize = (10,8))
fcast6.plot(ax = ax2, color = "blue")
fcast7.plot(ax = ax2, color = "red")
plt.show()


########################################################################
################# Train/Test split on Time Series #######################
#######################################################################

train = aep_mod.iloc[:84]
valid = aep_mod.iloc[84:88]
test = aep_mod.iloc[88:]

train_valid = aep_mod.iloc[:88]

fit_extra=ExponentialSmoothing(train['mw'],trend='add',seasonal='mul',seasonal_periods=12).fit()

fcast_extra = fit_extra.forecast(24)
error = valid['mw'] - fcast_extra
MAE = np.mean(abs(error))
print(MAE)

MAPE = np.mean(abs(error)/valid['mw'])
print(MAPE)

###########################################################################
####################### ARIMA #########################################
#########################################################################

adf_result =adfuller(aep_mod['mw'])
print(f'ADF p-value: {adf_result[1]}')

plot_acf(aep_mod['mw'], lags =12)
pyplot.show()

plot_pacf(aep_mod['mw'],lags = 12)
pyplot.show()


arima_one = ARIMA(aep_mod['mw'], order = (2,1,3))
a1fit = arima_one.fit()
print(a1fit.summary())


residuals = pd.DataFrame(a1fit.resid)
residuals.plot()
pyplot.show()

print(residuals.describe())

plot_acf(residuals, lags =12)
pyplot.show()

plot_pacf(residuals,lags = 12)
pyplot.show()


##### Ljung Box (first val is test-statistic, seond val is p-val)

lag_test=[6,7,8,9,10]

for x in lag_test:
  print(sm.stats.acorr_ljungbox(a1fit.resid, lags=[x], model_df=5)) #high p vals
  
  
######## Auto-ARIMA
model2 = pm.auto_arima(aep_mod['mw'], start_p=0,start_q=0,max_p=5,max_q=5,d = 1, seasonal=False) #can set d=0, but I chose not to here
model2.summary()
  