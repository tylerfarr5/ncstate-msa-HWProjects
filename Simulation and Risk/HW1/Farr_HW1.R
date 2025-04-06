library(tidyverse)
library(ggplot2)
library(readxl)
library(quantmod)
library(EnvStats)
library(ks)
library(nortest)
#####################################################################
######### Reading in the data ###################################
################################################################

price_proj <- read_xlsx("Analysis_Data.xlsx", range = "A3:D29")
drilling_costs <- read_xlsx("Analysis_Data.xlsx", sheet = 2, range = "A3:G51")

#removing 2007 because it's an outlier, trimming to 1991 - 2007
drilling_costs <- drilling_costs[32:47,]


#making the arithmetic returns as numeric
numeric_columns <- c("Arithmetic Return - Crude Oil", "Arithmetic Return - Natural Gas", "Arithmetic Return - Dry Well")

for (col in numeric_columns) {
  drilling_costs[[col]] <- as.numeric(drilling_costs[[col]])
}

str(drilling_costs)

drilling_costs$avgReturn <- apply(drilling_costs[5:7],1,mean)

################################################################
####### Simulating 2006 - 2023: Normal/Tri/Tri #####################
#############################################################

#Historical mean and sd = 0.1314913, 0.1282013
oscar_the_grouch <- mean(drilling_costs$avgReturn)
sup_dude <- sd(drilling_costs$avgReturn)

set.seed(144)
P_2012 <- rep(0,10000)
for(i in 1:10000){
  P0 = (2238.6 +1936.2+2664.6)/3 #2006 avg cost
  #P0 = drilling_costs$avgReturn[16]
  r <- rnorm(n=1, mean=oscar_the_grouch, sd=sup_dude)
  
  Pt <- P0*(1 + r)
  
  #2006-2012
  for(j in 1:5){
    r <- rnorm(n=1, mean=oscar_the_grouch, sd=sup_dude)
    Pt <- Pt*(1+r)
  }
  P_2012[i] <- Pt
  
  #2012-2015
  for(j in 1:3){
    r <- rtri(n = 1, min = 0.07, max = 0.22, mode = 0.0917)
    Pt <- Pt*(1-r)
  }
  P_2015[i] <- Pt
  
  #2015-2023
  for(j in 1:9){
    r <- rtri(n = 1, min = 0.02, max = 0.06, mode = 0.05)
    Pt <- Pt*(1 + r)
  }
  P_2023[i] <- Pt
}

hist(P_2023, breaks=50, main='Future Year Value Distribution (2006-2023)', xlab='Final Value')
abline(v = P0, col="red", lwd=2)
mtext("Initial Inv.", at=P0, col="red")

#QQ Plot to verify - not Normal
ggplot(data = as.data.frame(P_2023), aes(sample = P_2023)) +
  stat_qq() +
  stat_qq_line()

ad.test(P_2023)


###### P_2023 also includes 2024

###################################################################
################ Kernel Density Estimate ########################
##################################################################

hist(drilling_costs$avgReturn)

density.returns <- density(drilling_costs$avgReturn)
density.returns

#### simulating a KDE distribution
set.seed(144)
est.returns <- rkde(fhat=kde(avgReturns, h=density.returns$bw), n=10000)
hist(est.returns, breaks=50, main='KDE of Historical Arithmetic Returns', xlab='Oil Wazzzuppp')


##### re-doing the 2006-2012, but for the KDE
set.seed(144)
P_2012_kde <- rep(0,10000)
for(i in 1:10000){
  P0_kde = (2238.6 +1936.2+2664.6)/3 #2006 avg cost
  ##P0 = drilling_costs$avgReturn[16]
  r <- rkde(fhat=kde(avgReturns, h=density.returns$bw), n=10000)
  
  Pt_kde <- P0_kde*(1 + r)
  
  for(j in 1:5){
    r <- rkde(fhat=kde(avgReturns, h=density.returns$bw), n=10000)
    Pt_kde <- Pt_kde*(1+r)
  }
  P_2012_kde[i] <- Pt_kde
}

hist(P_2012_kde, breaks=50, main='Five Year Value Distribution (2006-2012)', xlab='Final Value')
abline(v = P0_kde, col="red", lwd=2)
mtext("Initial Inv.", at=P0_kde, col="red")


###################################################################
####### Simulating possible future values of 2024
####################################################################

