
library(factoextra)
library(FactoMineR)
library(clustertend)
library(ggplot2)
library(dplyr)
library(psych)
library(GPArotation)

setwd("/Users/eugenechua/Downloads")
caravan<-read.csv(file="caravan-insurance-challenge.csv",header=TRUE)

head(caravan)

caravantrain<-subset(caravan,ORIGIN=="train")
colnames(caravantrain)

#Checking for missing values
sapply(caravantrain,function(x)sum(is.na(x),na.rm=TRUE))

#Creating Customer main type dataframe
custMainType <- data.frame(MOSHOOFD=caravantrain$MOSHOOFD,CARAVAN=caravantrain$CARAVAN)

custMainType$CARAVAN <- as.factor(custMainType$CARAVAN)

custMainType$MOSHOOFD<-factor(custMainType$MOSHOOFD,levels=c(1:10),labels=c("Successful Hedonists","Driven Growers",
                                                                            "Average Family","Career Loner",
                                                                            "Living Well","Crusing Senior",
                                                                            "Retired and Religious","Family Grown Ups",
                                                                            "Conservative Families","Farmers"))

ggplot(custMainType,aes(x=reorder(MOSHOOFD,MOSHOOFD,function(x)-length(x)),fill=CARAVAN)) + geom_bar() +
  theme(axis.text.x=element_text(angle=90),panel.grid.major=element_blank()) + labs(x="Customer Main Types",title="Caravan across Cust Main Types")
#Very skewed dataset, largely with family with grown ups..

##Plotting CARAVAN across sub customer type
subcustType<-data.frame(MOSTYPE=caravantrain$MOSTYPE,CARAVAN=caravantrain$CARAVAN)
subcustType$CARAVAN<-as.factor(subcustType$CARAVAN)
subcustType$MOSTYPE<-as.factor(subcustType$MOSTYPE)

ggplot(subcustType,aes(x=reorder(MOSTYPE,MOSTYPE,function(x)-length(x)),fill=CARAVAN)) + geom_bar() +
  theme(panel.grid.major=element_blank()) + labs(x="Customer Subtypes",title="Caravan across Customer Subtypes")
##Dominated by lower class large family, then followed by a distant middle class family

##Plotting CARAVAN across Household size
houseSize<-data.frame(MGEMOMV=caravantrain$MGEMOMV,CARAVAN=caravantrain$CARAVAN)
houseSize$CARAVAN<-as.factor(houseSize$CARAVAN)
houseSize$MGEMOMV<-as.factor(houseSize$MGEMOMV)

#Creating the plot now
ggplot(houseSize,aes(x=reorder(MGEMOMV,MGEMOMV,function(x)-length(x)),fill=CARAVAN)) + geom_bar() +
  theme(panel.grid.major=element_blank()) + labs(x="Household size",title="Caravan across Household size")

#Subsetting the insurance statistics now
caravantrain2<-caravantrain[,45:86]

caravan.pca<-PCA(caravantrain2,scale.unit=TRUE,graph=TRUE,ncp=15)

#As mentioned previously, eigenvalues measure the amount of variation retained by each principal component
#Eigenvalues are large for the first PCs and then smaller for subsequent PCs.
get_eigenvalue(caravan.pca)

#This function allows me to better access the attributes better, like the loadings and cos2 (quality on the factormap)
caravan.var<-get_pca_var(caravan.pca)

#To look at the loadings on the PCs
caravan.var$coord

#Export the principal component loadings to csv
caravanPCALoadings<-as.data.frame(caravan.var$coord)

write.csv(caravanPCALoadings,file="CaravanPCA2.csv",row.names=TRUE)

#To look at principal component scores
caravan.pca$ind$coord

#Creating the correlation matrix between the variables, reason being we need to test for correlation adequacy
caravanCOR<-cor(caravantrain2,use="pairwise.complete.obs")

#Correlation adequacy Barlett Test
cortest.bartlett(caravanCOR,n=nrow(caravantrain2))
#Barlett Test is bloody significant!!! wohoooo

##How many factors to extract?
numberfactors<-fa.parallel(caravantrain2,fm="ml",fa="fa")
#Kaisen criterion
sum(numberfactors$fa.values>1)
##parallel analysis suggest 20, but kaisen criterion suggest 7....need to properly decide.
