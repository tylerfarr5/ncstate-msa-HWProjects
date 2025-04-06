library(tidyverse)
library(ggplot2)
library(readxl)
library(quantmod)
library(EnvStats)
library(ks)
library(nortest)
library(graphics)
library(quantmod)
library(TTR)
library(scales)
library(fExtremes)
library(truncnorm)
library(Rlab)
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


#########################################################################
############# HW2 ############
########################################################################

standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

sim_count <- 500000

Comb.Drilling <- drilling_costs$avgReturn


#Correlating Production amount and decline rate
R <- matrix(data=cbind(1,0.64, 0.64, 1), nrow=2)
U <- t(chol(R))

set.seed(229)
Production <- rlnorm(n=sim_count, meanlog=6, sdlog=0.28)
Decline <- sample(15:32, sim_count, replace = TRUE)
Both.r <- cbind(standardize(Decline), standardize(Production))

PD <- U %*% t(Both.r)
PD <- t(PD)

final.PD <- cbind(destandardize(PD[,1], Decline), destandardize(PD[,2], Production))


#Calculating wacc
WACC <- rep(0,15)
for (k in 1:15){
  WACC[k] <- (1+0.1)^k
}

#Simulation of 1 dry well and 1 wet well
Dry.Wells <- rep(0,sim_count)
NPV.Wet <- rep(0,sim_count)

wetwellcost <- rep(0, sim_count)

set.seed(229)
for (i in 1:sim_count){
  
  #Estimation of 2024 drilling costs
  P0 = (2238.6 +1936.2+2664.6)/3 #2006 avg cost
  r <- rnorm(n=1, mean=mean(Comb.Drilling), sd=sd(Comb.Drilling))
  
  Pt <- P0*(1 + r)
  
  #2007-2012
  for(j in 1:5){
    r <- rnorm(n=1, mean=mean(Comb.Drilling), sd=sd(Comb.Drilling))
    Pt <- Pt*(1+r)
  }

  #2012-2015
  for(j in 1:3){
    r <- rtri(n = 1, min = 0.07, max = 0.22, mode = 0.0917)
    Pt <- Pt*(1-r)
  }

  #2015-2023
  for(j in 1:9){
    r <- rtri(n = 1, min = 0.02, max = 0.06, mode = 0.05)
    Pt <- Pt*(1 + r)
  }
  
  Drilling.2024 <- Pt
  
  #Assumed Same Across Wells
  #Price of Oil & Operating Costs - Changes per Year
  Future.Prices <- rep(0,15)
  for (k in 1:15){
    Future.Prices[k] <- rtri(n = 1, min = price_proj$`Low Oil Price`[k], max = price_proj$`High Oil Price`[k], mode = price_proj$`AEO2023 Reference`[k])
  }
  
  Operating.Costs <- rep(0,15)
  for (k in 1:15){
    Operating.Costs[k] <- rnorm(n=1, mean = 2.25, sd = 0.3)
  }
  
  #Single Dry Well Calculation
  seismic <- rnorm(1, mean = 3, sd = 0.35)*43000 #PER WELL!
  lease <- rnorm(1, mean = 600, sd= 50)*960 #PER WELL!
  overhead <- rtri(1, min = 172000, max = 279500, mode = 215000)
  
  Drilling.Costs <- Drilling.2024*1000
  
  Dry.Wells[i] <- seismic + lease + overhead + Drilling.Costs
  
  
  #Single Wet Well Calculation
  #seismic <- rnorm(1, mean = 3, sd = 0.35)*43000 #PER WELL!
  #lease <- rnorm(1, mean = 600, sd= 50)*960 #PER WELL!
  completion <- rnorm(1, mean = 390000, sd = 50000)
  #overhead <- rtri(1, min = 172000, max = 279500, mode = 215000)
  #Drilling.Costs <- Drilling.2024*1000
  
  Wet.Wells0 <- seismic + lease + overhead + completion + Drilling.Costs
  
  Begin.Prod <- rep(0,15)
  End.Prod <- rep(0,15)
  for (k in 1:15){
    Begin.Prod[k] <- final.PD[i,2]*(1-final.PD[i,1]/100)^(k-1)
    End.Prod[k] <-  final.PD[i,2]*(1-final.PD[i,1]/100)^(k)
  }
  
  Annual.Vol <- 365*((Begin.Prod+End.Prod)/2)
  Revenue <- Annual.Vol*Future.Prices
  Total.Cost <- Operating.Costs*Annual.Vol + overhead
  
  NRI <- rnorm(n =1, mean = 0.75, sd = 0.02)
  Sev.Tax <- 0.046
  Revenue.NRI <- Revenue*NRI #FNR per year
  
  Net.Revenue.Taxed <- (Revenue.NRI- Total.Cost)*(1-Sev.Tax)
  
  NPV.Wet[i] <- sum(Net.Revenue.Taxed/WACC) - Wet.Wells0
  
  wetwellcost[i] <- Wet.Wells0 + sum(Total.Cost)

}


hist(NPV.Wet/1000000, breaks = 100, main = "Net Present Value of One Wet Well", xlab = "$ Millions")
med.wet <- median(NPV.Wet/1000000)
abline(v = med.wet, col = "red", lwd = 2)
mtext(paste("Median =", dollar(med.wet), "Million", sep = " "), at = med.wet, col = "red")

hist(Dry.Wells/1000000, breaks = 100, main = "Net Present Value (Cost) of One Dry Well", xlab = "$ Millions", xlim = c(0,20))
med.dry <- median(Dry.Wells/1000000)
abline(v = med.dry, col = "red", lwd = 2)
mtext(paste("Median =", dollar(med.dry), "Million", sep = " "), at = med.dry, col = "red")



#####################################################################
############## Probability of a producing well
#######################################################################

#P_pw = P_h x P_r x P_seal x P_Structure

p_structure <- 1
p_seal <- 1

wet_well_prob <- rep(0,sim_count)

total_npv <- rep(0, sim_count)

total_cost <- rep(0, sim_count)
#CHOCOLATE MILK!!!

set.seed(229)#leap year!!
for (j in 1:sim_count) {
  
  num_wells <- sample(10:30, 1, replace = TRUE) #number of wells
  well_status = rep(0, num_wells)
  
  #simulates 10-30 wells being dry or wet
  for (i in 1:num_wells) {
    p_hydrocarbon <- rtruncnorm(1, mean = 0.99, sd = 0.05, a = 0, b = 1)
    p_reservoir <- rtruncnorm(1, mean = 0.8, sd = 0.10, a = 0, b = 1)
    
    prod_well_prob <- p_structure*p_seal*p_hydrocarbon*p_reservoir
    #print(prod_well_prob)
    
    well_wet <- rbern(1, prod_well_prob) #p = prob of success
    
    well_status[i] <- well_wet
  }
  
  wet_well_prob[j] <- sum(well_status)/length(well_status)
  total_npv[j] <- sum(well_status)*NPV.Wet[j] - (length(well_status) - sum(well_status))*Dry.Wells[j]
  
  total_cost[j] <- sum(well_status)*wetwellcost[j] + (length(well_status) - sum(well_status))*Dry.Wells[j]
}

########### Histogram of total NPV over entire project
hist(total_npv/1000000, breaks = 100, main = "15-year Net Present Value of Entire Project (Dry & Wet Well)", xlab = "$ Millions")
med.all <- median(total_npv/1000000)
abline(v = med.all, col = "red", lwd = 2)
mtext(paste("Median =", dollar(med.all), "Million", sep = " "), at = med.all, col = "red")

#5% var
var5perc <- mean(total_npv) + qnorm(.05)*sd(total_npv)
var5perc 
#5% CVar
mean(total_npv[total_npv < var5perc]) 


#what was the total investment? wet + dry
hist(total_cost/1000000, breaks = 100, main = "15-year Cost of Entire Project (Dry & Wet Well)", xlab = "$ Millions")
med.cost <- median(total_cost/1000000)
abline(v = med.cost, col = "red", lwd = 2)
mtext(paste("Median =", dollar(med.cost), "Million", sep = " "), at = med.cost, col = "red")


############ Histogram of wet well probabilities + hydrocarbon and reservoir
hist(wet_well_prob, breaks = 20, main = "", xlab = "Wet Well Probability")
med.wetwell <- round(median(wet_well_prob),2)
abline(v = med.wetwell, col = "red", lwd = 2)
mtext(paste("Median =", med.wetwell, sep = " "), at = med.wetwell, col = "red")


#5% VaR -- 0.5957
var5perc <- mean(wet_well_prob) + qnorm(.05)*sd(wet_well_prob)
var5perc
#5% CVar -- 0.5432
cvar5perc <- mean(wet_well_prob[wet_well_prob < var5perc])
cvar5perc


#hydrocarbon and reservoir probs
p_h <- rep(0, sim_count)
p_r <- rep(0, sim_count)
set.seed(229)
for (i in 1:sim_count) {
  p_hydrocarbon <- rtruncnorm(1, mean = 0.99, sd = 0.05, a = 0, b = 1)
  p_reservoir <- rtruncnorm(1, mean = 0.8, sd = 0.10, a = 0, b = 1)
  
  p_h[i] <- p_hydrocarbon
  p_r[i] <- p_reservoir
}

#hydrocarbon
hist(p_h, xlab = "Probability of Hydrocarbon", main = "")

#reservoir
hist(p_r, xlab = "Probability of Reservoir Formation", main =)