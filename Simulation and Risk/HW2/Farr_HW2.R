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


#################################################################
########### Year 0 Costs
################################################################

#Historical mean and sd = 0.1314913, 0.1282013
oscar_the_grouch <- mean(drilling_costs$avgReturn)
sup_dude <- sd(drilling_costs$avgReturn)

year0_costs <- rep(0,10000)

set.seed(15955683)
for (i in 1:10000) {
  P0 = (2238.6 +1936.2+2664.6)/3 #2006 avg cost
  r <- rnorm(n=1, mean=oscar_the_grouch, sd=sup_dude)
  
  Pt <- P0*(1 + r)
  
  #2006-2012
  for(j in 1:5){
    r <- rnorm(n=1, mean=oscar_the_grouch, sd=sup_dude)
    Pt <- Pt*(1+r)
  }
  #P_2012[i] <- Pt
  
  #2012-2015
  for(j in 1:3){
    r <- rtri(n = 1, min = 0.07, max = 0.22, mode = 0.0917)
    Pt <- Pt*(1-r)
  }
  #P_2015[i] <- Pt
  
  #2015-2023
  for(j in 1:9){
    r <- rtri(n = 1, min = 0.02, max = 0.06, mode = 0.05)
    Pt <- Pt*(1 + r)
  }
  #P_2023[i] <- Pt
  
  seismic <- rnorm(1, mean = 3, sd = 0.35)*43000 #PER WELL!
  lease <- rnorm(1, mean = 600, sd= 50)*960 #PER WELL!
  completion <- rnorm(1, mean = 390000, sd = 50000)
  overhead <- rtri(1, min = 172000, max = 279500, mode = 215000)
  
  year0_costs[i] <- Pt + seismic + lease + completion + overhead
}

summary(year0_costs)

hist(year0_costs, breaks = 50, main = '2024 Total Cost Distribution (Dry Well)', xlab = 'Total Costs')

######## seismic and lease costs
#seismic cost per section = $43,000
#lease costs per acre = $960

######## completion costs
#IF oil is present & no dry well hole drilled yet

######## professional overhead
# stops after year 0 if well is dry


##############################################################
####### Wet Well
##############################################################



#################################################################
########## Production Risk = IP and Decline Rate
#################################################################

standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}


#Historical mean and sd = 0.1314913, 0.1282013
oscar_the_grouch <- mean(drilling_costs$avgReturn)
sup_dude <- sd(drilling_costs$avgReturn)

Oil_Vol <- rep(0,15)
PROD.r.bal <- rep(0,10000)

R <- matrix(data=cbind(1,0.64, 0.64, 1), nrow=2)
U <- t(chol(R))

rev_annual <- rep(0,15)

wacc <- 0.10

set.seed(16955683)
IP.r <- rlnorm(n=15, meanlog=6, sdlog=0.28)
set.seed(16955683)
DR.r <- runif(n = 15, min = 0.15, max= 0.32)

Both.r <- cbind(standardize(IP.r), standardize(DR.r))
IP_DR.r <- U %*% t(Both.r)
IP_DR.r <- t(IP_DR.r)
final.IP_DR.r <- cbind(destandardize(IP_DR.r[,1], IP.r), destandardize(IP_DR.r[,2], DR.r))


severance_taxes <- 0.046

NPVyaya <- rep(0,10000)

set.seed(16955683)
for (i in 1:10000) {
  P0 = (2238.6 +1936.2+2664.6)/3 #2006 avg cost
  r <- rnorm(n=1, mean=oscar_the_grouch, sd=sup_dude)
  
  Pt <- P0*(1 + r)
  
  #2006-2012
  for(j in 1:5){
    r <- rnorm(n=1, mean=oscar_the_grouch, sd=sup_dude)
    Pt <- Pt*(1+r)
  }
  #P_2012[i] <- Pt
  
  #2012-2015
  for(j in 1:3){
    r <- rtri(n = 1, min = 0.07, max = 0.22, mode = 0.0917)
    Pt <- Pt*(1-r)
  }
  #P_2015[i] <- Pt
  
  #2015-2023
  for(j in 1:9){
    r <- rtri(n = 1, min = 0.02, max = 0.06, mode = 0.05)
    Pt <- Pt*(1 + r)
  }
  #P_2023[i] <- Pt
  
  seismic <- rnorm(1, mean = 3, sd = 0.35)*43000 #PER WELL!
  lease <- rnorm(1, mean = 600, sd= 50)*960 #PER WELL!
  completion <- rnorm(1, mean = 390000, sd = 50000)
  overhead <- rtri(1, min = 172000, max = 279500, mode = 215000)
  
  year0_costs <- Pt*1000 + seismic + lease + completion + overhead
  
  #Oil Production
  for(j in 1:15){
    if(j == 1){
      rateBegin = final.IP_DR.r[j,1] #pull from decomposed values
      declineRate = final.IP_DR.r[j,2] #pull from decomposed values
      rateEnd = (1 - declineRate) * rateBegin
      
      rateBegin = rateEnd 
      
      oilVol = 365 * ((rateBegin + rateEnd) / 2)
      Oil_Vol[j] = oilVol
    }
    
    #if this isn't the first year, you want to use previous year end to calcuate new year end
    else{
      rateEnd = (1 - declineRate) * rateBegin
      oilVol = 365 * ((rateBegin + rateEnd) / 2)
      
      
      Oil_Vol[j] = oilVol
      rateBegin = rateEnd
    }
  }

  #Revenue Risk
  net_revenue_interest <- rnorm(n = 1, mean = 0.75, sd = 0.02)
  
  for (j in 1:15){ #lightning mcqueen cause Rev rev reevvvvvvv
    lightning_mcqueen <- rtri(n = 1, min = price_proj[j,3], max = price_proj[j,2], mode = price_proj[j,4])

    operating_costs <- rnorm(n=1, mean = 2.25, sd = 0.3)
    
    total_rev <- (lightning_mcqueen*Oil_Vol[j])*net_revenue_interest
    total_rev <- total_rev*(1-severance_taxes)
    total_rev <- total_rev - operating_costs*Oil_Vol[j] - overhead
    
    rev_annual[j] <-total_rev
    
  }
  
  NPV <- 0
  for (j in 1:15) {
    NPV <- NPV + (rev_annual[j]/((1+wacc)^j))
  }
  
  NPV <- NPV - year0_costs
  
  NPVyaya[i] <- NPV
  
  
}

hist(NPVyaya)

