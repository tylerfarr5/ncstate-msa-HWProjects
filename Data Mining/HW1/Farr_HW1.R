library(datasets)
library(arules)
library(arulesViz)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(TH.data)
library(ISLR2)
library(lattice)
library(stats)
library(rattle)
library(RColorBrewer)
library(caret)
library(ROCR)
library(tidyverse)  
library(cluster)  
library(factoextra) 
library(gridExtra)
library(NbClust)
library(dendextend)
library(class)
library(ClustOfVar)
library(MASS)
library(kableExtra)
library(partykit)
library(dbscan)
###############################################################
############### Reading in the data - Pivot Wider Method ############
#############################################################
wine <- read.csv("https://raw.githubusercontent.com/sjsimmo2/DataMining-Fall/master/orderData.csv")

#classify rows as meat/drink/side to pivot
type <-rep(c("meat", "drink", "side"), nrow(wine)/3)
wine$type <- type

#pivots long to wide
wine <- wine %>%
  pivot_wider(names_from = type, values_from = item)

###############################################################################
########################### Summary Statistics on meat/wine/side #############
##########################################################################
meat_freq <- wine %>%
  group_by(meat) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 

meat_freq

ggplot(data = meat_freq, aes(x = reorder(meat,-count), y= count)) + 
  geom_bar(stat = 'identity', fill = "blue") +
  labs(x = "Type of Meat", y = "Count", title = "Type of Meat Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))
  

drink_freq <- wine %>%
  group_by(drink) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

drink_freq

ggplot(data = drink_freq, aes(x = reorder(drink,-count), y = count)) +
  geom_bar(stat = 'identity', fill = "red") +
  labs(x = "Type of Wine", y = "Count", title = "Type of Wine Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))

side_freq <- wine %>%
  group_by(side) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

side_freq

ggplot(data = side_freq, aes(x = reorder(side,-count), y = count)) +
  geom_bar(stat = 'identity', fill = "green") +
  labs(x = "Type of Sides", y = "Count", title = "Type of Sides Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))


########## looks at each table/party most common meat purchase
table_meat <- wine %>%
  select(orderNo, meat) %>%
  group_by(orderNo, meat) %>%
  mutate(count= n()) %>%
  unique() %>%
  pivot_wider(names_from = meat, values_from = count, values_fill = 0)

View(table_meat)
########### looks at most popular combos of meat/wine
wine %>%
  group_by(meat,drink) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

############################################################################
######################## Reading in data - as() method ##################
###########################################################################

wine2 <- read.csv("https://raw.githubusercontent.com/sjsimmo2/DataMining-Fall/master/orderData.csv")

#wide to long
wine2$ID <- paste(wine2$orderNo, wine2$seatNo, sep = "_")
wine.dat <- as(split(wine2$item, wine2$ID), "transactions")

inspect(wine.dat)

#look at unique values within
wine.dat@itemInfo$labels

#top 7 meat, wine, or sides ordered
itemFrequencyPlot(wine.dat, topN = 7, type = "absolute")

meat_options <- c("Filet Mignon", "Sea Bass", "Pork Tenderloin", "Pork Chop", "Salmon", "Duck Breast", "Swordfish", "Roast Chicken")

for (i in meat_options) {
  rules <- apriori(wine.dat, parameter = list(supp = 0.00001, 
                                              conf = 0.00001,
                                              target = "rules", 
                                              minlen = 2),
                   appearance = list(lhs = i,
                                     default = "rhs"))
  
  rules <- sort(rules, by = "confidence", decreasing = TRUE)
  
  inspect(rules[1:7])
}

# Filet Mignon ==> Blackstone Merlot. 
#####Support = 0.0627. See Filet Mignon and Blackstone Merlot in 6.3% of the dataset
##### Lift = 3.2. Customer is 3.2 times more likely to buy a Blackstone Merlot with a Filet Mignon than customers randomly ordering Blackstone Merlot
##### Confidence = 0.357. Of the purchases containing Filet Mignon, 35.7% of them bought Blackstone Merlot

# Sea Bass ==> Innocent Bystander Sauvignon Blanc (option 2). 
#####Support = 0.0367. See Sea Bass and Innocent Bystander Sauvignon Blanc in 3.7% of the dataset
##### Lift = 2.7. Customer is 2.7 times more likely to buy a Innocent Bystander Sauvignon Blanc with a Sea Bass than customers randomly ordering Innocent Bystander Sauvignon Blanc
##### Confidence = 0.227. Of the purchases containing Sea Bass, 22.7% of them bought Innocent Bystander Sauvignon Blanc

# Pork Tenderloin ==> Adelsheim Pinot Noir. 
#####Support = 0.044. See Pork Tenderloin and Adelsheim Pinot Noir in 4.4% of the dataset
##### Lift = 2.21. Customer is 2.2 times more likely to buy a Adelsheim Pinot Noir with a Pork Tenderloin than customers randomly ordering Adelsheim Pinot Noir
##### Confidence = 0.299. Of the purchases containing Pork Tenderloin, 29.9% of them bought Adelsheim Pinot Noir

# Pork Chop ==> Duckhorn Chardonnay 
#####Support = 0.033. See Pork Chop and Duckhorn Chardonnay in 3.3% of the dataset
##### Lift = 1.49. Customer is 1.5 times more likely to buy a Duckhorn Chardonnay with a Pork Chop than customers randomly ordering Duckhorn Chardonnay 
##### Confidence = 0.229. Of the purchases containing Pork Chop, 22.9% of them bought Duckhorn Chardonnay 

# Salmon ==> Oyster Bay Sauvignon Blanc. 
#####Support = 0.018. See Salmon and Oyster Bay Sauvignon Blanc in 1.8% of the dataset
##### Lift = 2.37. Customer is 2.4 times more likely to buy a Oyster Bay Sauvignon Blanc with a Salmon than customers randomly ordering Oyster Bay Sauvignon Blanc
##### Confidence = 0.150. Of the purchases containing Salmon, 15.0% of them bought Oyster Bay Sauvignon Blanc

# Duck Breast ==> Blackstone Merlot. 
#####Support = 0.033. See Duck Breast and Blackstone Merlot in 3.3% of the dataset
##### Lift = 2.85. Customer is 2.9 times more likely to buy a Blackstone Merlot with a Duck Breast than customers randomly ordering Blackstone Merlot
##### Confidence = 0.317. Of the purchases containing Duck Breast, 31.7% of them bought Blackstone Merlot

# Swordfish ==> Total Recall Chardonnay. 
#####Support = 0.0174. See Swordfish and Total Recall Chardonnay in 1.7% of the dataset
##### Lift = 1.70. Customer is 1.7 times more likely to buy a Total Recall Chardonnay with a Swordfish than customers randomly ordering Total Recall Chardonnay
##### Confidence = 0.179. Of the purchases containing Swordfish, 17.9% of them bought Total Recall Chardonnay

# Roast Chicken ==> Oyster Bay Sauvignon Blanc. --> could argue no suggestion here
#####Support = 0.0096. See Roast Chicken and Oyster Bay Sauvignon Blanc in 0.96% of the dataset
##### Lift = 3.07. Customer is 3.07 times more likely to buy a Oyster Bay Sauvignon Blanc with a Roast Chicken than customers randomly ordering Oyster Bay Sauvignon Blanc
##### Confidence = 0.194. Of the purchases containing Roast Chicken, 19.4% of them bought Oyster Bay Sauvignon Blanc


###########################################################################
######################## Other findings ###################################
###########################################################################

meat_options <- c("Filet Mignon", "Sea Bass", "Pork Tenderloin", "Pork Chop", "Salmon", "Duck Breast", "Swordfish", "Roast Chicken")
side_options <- c("Seasonal Veg", "Bean Trio", "Roasted Root Veg", "Warm Goat Salad", "Roasted Potatoes", "Caesar Salad", "Mashed Potatoes")
for (i in meat_options) {
  rules <- apriori(wine.dat, parameter = list(supp = 0.0001, 
                                              conf = 0.0001,
                                              target = "rules", 
                                              minlen = 2),
                   appearance = list(lhs = i,
                                     rhs= side_options))
  
  rules <- sort(rules, by = "confidence", decreasing = TRUE)
  
  inspect(rules[1:5])
}

#Pork Tenderloin --> Caesar Salad
#Support = 0.032
#Confidence = 0.222
#Lift = 1.84

# {Blackstone Merlot,Seasonal Veg} --> Filet Mignon
#Support = 0.0116
#Confidence = 0.571
#Lift = 3.24

#Filet mignon --> Adelsheim Pinot Noir
#Support = 0.0497
#Confidence = 0.2823
#Lift = 2.09

fish <- c("Sea Bass", "Salmon", "Swordfish")

wine$me <- ifelse(wine$meat %in% fish, "Seafood", "Meat")

ggplot(data = wine, aes(x = me, fill = side)) +
  geom_histogram(stat = 'count') 


table_meat$rowTotal <- rowSums(table_meat[,2:9])

write.csv(table_meat %>%
  filter(rowTotal >= 3) %>%
  group_by(rowTotal), "hello.csv")
