
library(factoextra)
library(FactoMineR)
library(clustertend)
library(ggplot2)
library(dplyr)
library(psych)
library(GPArotation)
library(reshape2)

setwd("C:/Users/Alvin/Desktop")

caravan1<-read_excel("Dataset_15PC.xlsx")

# remove repeated entry
caravan1 = caravan1 %>%
  select(-MGEMLEEF)
set.seed(123)
inds = createDataPartition(caravan1$CARAVAN, p=0.5, list=FALSE,times=1)

caravanTrain = caravan1[inds,]
nrow(trainset)/nrow(caravan1)

caravanTest = caravan1[-inds,]
nrow(testset)/nrow(caravan1)

#K medoid on train set
set.seed(1234)
pam.train<-eclust(caravanTrain[,c(93:107)],"pam",k=3,graph=FALSE)
print(pam.train)
table(pam.train$cluster)

plotcluster(caravanTrain[,c(93:107)],pam.train$cluster,main="3 cluster solution")
fviz_silhouette(pam.train,palette="jco",ggtheme=theme_classic())

caravanTrain$KMEDOID_4<-pam.train$cluster

#K medoid on test set
set.seed(1234)
pam.test<-eclust(caravanTest[,c(93:107)],"pam",k=3,graph=FALSE)
print(pam.test)
table(pam.test$cluster)

plotcluster(caravanTest[,c(93:107)],pam.test$cluster,main="3 cluster solution")
fviz_silhouette(pam.test,palette="jco",ggtheme=theme_classic())

caravanTest$KMEDOID_4<-pam.test$cluster

