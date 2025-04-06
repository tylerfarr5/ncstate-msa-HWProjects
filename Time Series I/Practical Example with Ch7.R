consume=read.csv("https://raw.githubusercontent.com/sjsimmo2/TimeSeries/master/consume1982.csv")
library(forecast)
library(ggplot2)
library(zoo)
library(aTSA)

income.ts=ts(consume$Disposable_income,start=c(1982,9),frequency = 12)

income.train=income.ts[1:70]
income.valid=income.ts[71:82]
income.test=income.ts[83:94]

autoplot(income.ts)
aTSA::adf.test(income.train,nlag=3)
model1=auto.arima(income.train,seasonal = F)
summary(model1)

ggAcf(model1$residuals,lag.max = 12)
ggPacf(model1$residuals,lag.max = 12)

box.pvalue=rep(NA,11)
for (i in 2:11)
{ box.pvalue[i]=Box.test(model1$residuals,lag = i,type="Ljung-Box",fitdf=1)$p.value}

white.dat=data.frame(cbind(box.pvalue[2:11],seq(2,11)))
colnames(white.dat)=c("pvalues","Lag") 


ggplot(white.dat,aes(x=factor(Lag),y=pvalues))+geom_col()+labs(title="Model 1",x="Lags",y="p-values")
income.plot=data.frame(income.train)
ggplot(data=income.plot,aes(x=residuals(model1)))+geom_histogram()


diff.train=diff(income.train,lag=1)
ggAcf(diff.train,lag.max=12)
ggPacf(diff.train,lag.max=12)

model2=Arima(income.train,order=c(0,1,6),include.drift = T, fixed=c(NA,0,0,0,0,NA,NA))
summary(model2)

ggAcf(model2$residuals,lag.max = 12)
ggPacf(model2$residuals, lag.max = 12)

box.pvalue=rep(NA,11)
for (i in 7:12)
{ box.pvalue[i]=Box.test(model2$residuals,lag = i,type="Ljung-Box",fitdf=6)$p.value}

white.dat=data.frame(cbind(box.pvalue[7:11],seq(7,11)))
colnames(white.dat)=c("pvalues","Lag") 


ggplot(white.dat,aes(x=factor(Lag),y=pvalues))+geom_col()+labs(title="Model 2",x="Lags",y="p-values")

ggplot(data=income.plot,aes(x=residuals(model2)))+geom_histogram()


model1.for=forecast::forecast(model1,h=12)
model2.for=forecast::forecast(model2,h=12)

MAPE.1=mean(abs(income.valid-model1.for$mean)/income.valid)
MAPE.2=mean(abs(income.valid-model2.for$mean)/income.valid)

mean(abs(income.valid-model1.for$mean))
mean(abs(income.valid-model2.for$mean))