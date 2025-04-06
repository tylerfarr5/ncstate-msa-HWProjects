#title: simulation and risk hw1
#name: zakia ishaque

########## imports #########
library(quantmod)
library(ks)
library(dplyr)
library(stringr)
library(triangle)

######### data ###########

drillingcosts = read.csv('/Users/zakia/Desktop/Spring 1/Simulation and Risk/hw/drillingcosts.csv')
priceprojections = read.csv('/Users/zakia/Desktop/Spring 1/Simulation and Risk/hw/priceprojections.csv')

colnames(drillingcosts) = drillingcosts[2,]
drillingcosts = drillingcosts[34:49,]

drillingcosts = drillingcosts %>%
  mutate(`Arithmetic Return - Crude Oil` = str_replace(`Arithmetic Return - Crude Oil`, '%', '')) %>%
  mutate(`Arithmetic Return - Dry Well` = str_replace(`Arithmetic Return - Dry Well`, '%', '')) %>%
  mutate(`Arithmetic Return - Natural Gas` = str_replace(`Arithmetic Return - Natural Gas`, '%', '')) %>%
  mutate(across(colnames(drillingcosts), ~as.numeric(.))) %>%
  rowwise() %>%
  mutate(avgreturn = mean(c(`Arithmetic Return - Crude Oil`, `Arithmetic Return - Dry Well`, `Arithmetic Return - Natural Gas`))) %>%
  mutate(avgcost = mean(c(`U.S. Nominal Cost per Crude Oil Well Drilled (Thousand Dollars per Well)`, `U.S. Nominal Cost per Natural Gas Well Drilled (Thousand Dollars per Well)`, `U.S. Nominal Cost per Dry Well Drilled (Thousand Dollars per Well)`)))

colnames(drillingcosts) = c('date', 'crudeCost', 'natCost', 'dryCost', 'crudeReturn',
                            'natReturn', 'dryReturn', 'avgReturn', 'avgCost')

drillingcosts = as.data.frame(drillingcosts)

colnames(priceprojections) = priceprojections[2,]
priceprojections = priceprojections[3:28,]

priceprojections = priceprojections %>%
  mutate(across(colnames(priceprojections), ~as.numeric(.)))

colnames(priceprojections) = c('year', 'high', 'low', 'ref')

######## simulations #########

hist(drillingcosts$avgReturn) 

qqnorm(drillingcosts$avgReturn)
qqline(drillingcosts$avgReturn)
shapiro.test(drillingcosts$avgReturn) #normal

set.seed(144)

density(drillingcosts$avgReturn)

estDrillingCost = rkde(fhat=kde(drillingcosts$avgReturn, h = 4.408), n=10000000)

hist(estDrillingCost, breaks=50, main='KDE of Average Return for Drilling Costs (%)',
     xlab='Drilling Costs Average Return (%)')

r = estDrillingCost
p0 = as.numeric(drillingcosts[16,9])
p1 = p0*r

hist(p1, breaks=50, main='Average Drilling Cost', xlab='Final Value')
abline(v=as.numeric(drillingcosts[16,9]), col='red', lwd=2)
mtext('2006 Average Cost.', at=as.numeric(drillingcosts[16,9]), col='red')

#normal dist
mean = mean(drillingcosts$avgReturn)
stdev = sd(drillingcosts$avgReturn)

p30 = rep(0,10000)

for(i in 1:10000){
  p0 = as.numeric(drillingcosts[16,9])
  r = (rnorm(n=1, mean = mean, sd=stdev))/100
  pt = p0*(1+r)
  
  for(j in 1:6){
    r <- (rnorm(n=1, mean=mean, sd=stdev))/100
    pt <- pt*(1+r)
  }
  
  for(j in 1:3){
    r = rtriangle(n=1, a=-0.22, b=-0.07, c=-0.0917)
    pt = pt*(1+r)
  }
  
  for(j in 1:8){
    r = rtriangle(n=1, a=0.02, b=0.06, c=0.05)
    pt = pt*(1+r)
  }
  
  p30[i] <- pt
}

summary(p30)

hist(p30, breaks=50, main='2024 Value Distribution', xlab='Final Value')
abline(v = as.numeric(drillingcosts[16,9]), col="red", lwd=2)
mtext("2006 Cost", at=as.numeric(drillingcosts[16,9]), col="red")

#kernel density est
kern30 = rep(0,10000)

for(i in 1:10000){
  p0 = as.numeric(drillingcosts[16,9])
  r = (rkde(fhat=kde(drillingcosts$avgReturn, h = 4.408), n=1))/100
  pt = p0*(1+r)
  
  for(j in 1:6){
    r <- (rkde(fhat=kde(drillingcosts$avgReturn, h = 4.408), n=1))/100
    pt <- pt*(1+r)
  }
  
  for(j in 1:3){
    r = rtriangle(n=1, a=-0.22, b=-0.07, c=-0.0917)
    pt = pt*(1+r)
  }
  
  for(j in 1:8){
    r = rtriangle(n=1, a=0.02, b=0.06, c=0.05)
    pt = pt*(1+r)
  }
  
  kern30[i] <- pt
}

summary(kern30)

hist(kern30, breaks=50, main='2024 Value Distribution', xlab='Final Value')
abline(v = as.numeric(drillingcosts[16,9]), col="red", lwd=2)
mtext("Initial Inv.", at=as.numeric(drillingcosts[16,9]), col="red")

ad.test(kern30)

wilcox.test(p30, kern30)
