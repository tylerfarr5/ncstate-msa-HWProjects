library(ggplot2)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(TH.data)
library(lattice)
library(stats)
library(rattle)
library(RColorBrewer)
library(caret)
library(ROCR)
library(MASS)
library(factoextra)
library(cluster)
library(NbClust)
library(dbscan)
library(kable)
library(fastDummies)
##############################################################
############ Importing Data #################################
#############################################################

load("C:/Users/thfar/OneDrive/Documents/IAA/IAA - AA502/Data Mining/Clustering Lab/TeenSNS4.RData")

summary(teens4)
str(teens4)
dim(teens4)

colSums(is.na(teens4))

cont_var = c( "friends",      "basketball",   "football",     "soccer",
              "softball",     "volleyball",   "swimming",     "cheerleading", "baseball",     "tennis",       "sports",
              "cute",         "sex",          "sexy",         "hot",          "kissed",       "dance",        "band",
              "marching",     "music",        "rock",         "god",          "church",       "jesus",        "bible",
              "hair",         "dress",        "blonde",       "mall",         "shopping",     "clothes",      "hollister",
              "abercrombie",  "die",          "death",        "drunk",        "drugs")
gather(teens4) %>%
  filter(key %in% cont_var) %>%
  ggplot(aes(value)) +
  geom_histogram(stat="count") +
  facet_wrap(~key, scales = 'free_x')

#replacing NA with 'missing' values
teens4$gender <- ifelse(is.na(teens4$gender), "Missing", teens4$gender)
teens4$gender <- as.factor(teens4$gender)

########################################################
############### Normalize + Standardize ##############
############################################################

##### Normalize (since data is skewed)
for (i in cont_var) {
  teens4[[i]] = ifelse(teens4[[i]] == 0, 0.01, teens4[[i]])
}

teens4[4:40] <- log(teens4[4:40])

hist(teens4$basketball)

###### standardize

# scale
teens4[,c(1, 3:40)] <- scale(teens4[,c(1, 3:40)])

# z score


##############################################################################
##################### Visualize Processed Data #############################
##############################################################################
cont_var = c( "friends",      "basketball",   "football",     "soccer",
              "softball",     "volleyball",   "swimming",     "cheerleading", "baseball",     "tennis",       "sports",
              "cute",         "sex",          "sexy",         "hot",          "kissed",       "dance",        "band",
              "marching",     "music",        "rock",         "god",          "church",       "jesus",        "bible",
              "hair",         "dress",        "blonde",       "mall",         "shopping",     "clothes",      "hollister",
              "abercrombie",  "die",          "death",        "drunk",        "drugs")
gather(teens4) %>%
  filter(key %in% cont_var) %>%
  ggplot(aes(value)) +
  geom_histogram(stat="count") +
  facet_wrap(~key, scales = 'free_x')

hist(teens4$friends)

#############################################################################
################# Various clustering algorithms ###########################
###########################################################################

#K means
#2 clusters has WSS = 5.7%, tot = 1103084
#3 clustesr has WSS = 8.9%, tot = 1065417
#4 clusters has WSS = 11.7%, tot = 1033406
#5 clusters has WSS = 14.3%, tot = 1003083
#6 clusters has WSS = 15.9%, tot = 979149.5
#7 clusters has WSS = 17.9%, tot = 960905.9
#goes up with more clusters
clus2=kmeans(teens4[c(1,3:40)],centers=5,nstart = 25) #centers = num clusters
clus2
clus2$tot.withinss

fviz_cluster(clus2, data = teens4[c(1,3:40)]) #will be a blob if n is large. Try sampling if want to visualize with lots of data


### Option 1 to determine # of clusters to use -- look for ELBOW
set.seed(12964)
fviz_nbclust(teens4[c(1,3:40)], kmeans, method = "wss",k.max = 9)

#Option 2 - Silhoutte Method
fviz_nbclust(teens4[c(1,3:40)], kmeans, method = "silhouette",k.max = 9)

#Option 3 - Cluster Gap Statistic
set.seed(123)
gap_stat = clusGap(teens4[c(1,3:40)], FUN = kmeans, nstart = 25, K.max = 9, B = 50)
fviz_gap_stat(gap_stat)


#Profile each cluster (2 clusters chosen)
k2 <-kmeans(arrest.scal, centers =2, nstart =25)
profile.kmeans=cbind(USArrests,k2$cluster)

all.k=profile.kmeans %>% 
  group_by(k2$cluster) %>% 
  summarise(mean.assault=mean(Assault),
            mean.murder=mean(Murder),
            mean.rape=mean(Rape),
            mean.pop=mean(UrbanPop))

all.k

#An alternative library that outputs 24 measures and what they propose as best number of clusters (takes a looooong time to run tho!!!)
#NbClust(teens4[c(1,3:40)],method="kmeans",min.nc=2,max.nc = 4)


#DBSCAN

scan1<-hdbscan(teens4[c(1,3:40)],minPts=4)
pca_ex=prcomp(teens4[c(1,3:40)],scale=F)
scan1data=cbind.data.frame(pca_ex$x[,1],pca_ex$x[,2],as.factor(scan1$cluster+1))
colnames(scan1data)=c("PCA1","PCA2","cluster")
ggplot(scan1data,aes(x=PCA1,y=PCA2,color=cluster))+
  geom_point()+ 
  scale_fill_brewer(palette = "Dark2")




#########################################################
################### PCA ################################
load("C:/Users/thfar/OneDrive/Documents/IAA/IAA - AA502/Data Mining/Clustering Lab/TeenSNS4.RData")

#handling categorical values
str(teens4)
teens4$gradyear <- as.factor(teens4$gradyear)
teens4$gender <- ifelse(is.na(teens4$gender), "Missing", teens4$gender)
teens4$gender <- ifelse(teens4$gender == 2, "Male", ifelse(teens4$gender == 1, "Female", "Missing"))
teens4$gender <- as.factor(teens4$gender)

#creating dummy variables
new_df <- fastDummies::dummy_cols(teens4, remove_first_dummy = TRUE)


#correlation, diff scale
pca_cor <- prcomp(new_df[3:45], scale = FALSE) #correlation, diff scale
summary(pca_cor)

pca_cor_df <- data.frame(pca_cor$x)[1:2] #first 2 PC's explain most of the variation
ggplot(data = pca_cor_df, aes(x = PC1, y = PC2)) +
  geom_point()

clus2=kmeans(pca_cor_df,centers=3,nstart = 25) #centers = num clusters
clus2
clus2$tot.withinss

fviz_cluster(clus2, data = pca_cor_df) #will be a blob if n is large. Try sampling if want to visualize with lots of data

##############################################################################
#######################################################
#covariance, same scale
pca_cov <- prcomp(teens4[new_df[3:45], scale = TRUE) #covariance, same scale
summary(pca_cov)

pca_cov_df <- data.frame(pca_cov$x)
ggplot(data = pca_cov_df, aes(x = PC1, y = PC2)) +
  geom_point()

clus2=kmeans(pca_cov_df,centers=7,nstart = 25) #centers = num clusters
clus2
clus2$tot.withinss
#k = 2 4.9%, tot = 1112919
#k = 3 8.3%, tot = 1072283
#k = 4 10.6%, tot = 1045596
#k = 5 12.8%, tot = 1019772
#k = 6 14.9%, tot = 995923.8
#k = 7 16.6%, tot = 975325.5
##############################################################
###################################################################################


#Profile each cluster (2 clusters chosen)
#k2 <-kmeans(arrest.scal, centers =2, nstart =25)
profile.kmeans=cbind(new_df[3:45], clus2$cluster)

all.k=profile.kmeans %>% 
  group_by(clus2$cluster) %>% 
  summarise(across(everything(), list(mean)))

View(all.k)
