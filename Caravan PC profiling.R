

library(factoextra)
library(FactoMineR)
library(clustertend)
library(ggplot2)
library(dplyr)
library(psych)
library(GPArotation)
library(reshape2)

setwd("/Users/eugenechua/Downloads/Caravan_Insurance")
caravan<-read.csv(file="caravan-insurance-challenge.csv",header=TRUE)

head(caravan)

#Subsetting the insurance statistics now
caravan2<-caravan[,45:86]

#Melting the dataset to create the correlation plot
insuranceCOR<-round(cor(caravan2),2)
insuranceCOR<-melt(insuranceCOR,na.rm=TRUE)

ggplot(insuranceCOR,aes(x=Var1,y=Var2,fill=value)) + geom_tile() + scale_fill_gradient2(low="blue",high="orange",mid="white",limit=c(-1,1),name="Pearson\nCorrelation") +
  theme(axis.text.x=element_text(angle=90,size=9)) + coord_fixed() + labs(title="Correlation Plot")




setwd("/Users/eugenechua/Downloads/Caravan_Insurance")
list.files()

alvinsoln<-read.csv(file="clusterdata.csv",header=TRUE)
head(alvinsoln)

clusterVSpc<-aggregate(alvinsoln[,c(48:62)],list(ClusterNo=alvinsoln$Cluster),mean)

#Alternatively doing dplyr way
alvinsoln[,c(48:63)]%>%
  group_by(Cluster)%>%
  summarise_all(list(mean))

write.csv(clusterVSpc,file="PCProfiling.csv",row.names=FALSE)

