pacman::p_load(tidyverse,ggplot2,readxl,reshape2)
setwd("C:/Users/May Khoo/Documents/GitHub/Cust-Analytics")

caravan = read.csv("Dataset/Dataset_clean_v3.csv")

summary(caravan)
caravan$CARAVAN=as.factor(caravan$CARAVAN)
caravan$MOSTYPE=as.factor(caravan$MOSTYPE)
caravan$MAANTHUI=as.factor(caravan$MOSTYPE)
caravan$MOSHOOFD=as.factor(caravan$MOSHOOFD)

#Subsetting the insurance statistics now
caravan2<-read.csv(file="Dataset/caravan-insurance-challenge.csv",header=TRUE)

head(caravan2)

#Subsetting the insurance statistics now
caravan3<-caravan2[,45:86]

#Melting the dataset to create the correlation plot
insuranceCOR<-round(cor(caravan3),2)
insuranceCOR<-melt(insuranceCOR,na.rm=TRUE)

#Get Correlation Plot
ggplot(insuranceCOR,aes(x=Var1,y=Var2,fill=value)) + geom_tile() + scale_fill_gradient2(low="blue",high="orange",mid="white",limit=c(-1,1),name="Pearson\nCorrelation") +
  theme(axis.text.x=element_text(angle=90,size=9)) + coord_fixed() + labs(title="Correlation Plot")

#plot the number of caravan insurance holders

plot = caravan %>%
  group_by(CARAVAN) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=CARAVAN,y=count,fill=CARAVAN)) +
  geom_bar(stat="identity") +
  labs(x="Number of Caravan Insurance Holders") +
  geom_text(aes(label=count), vjust = -0.5, position = position_stack()); plot

#plot the number of caravan insurers/type of customer
plot2 = caravan %>%
  group_by(MOSHOOFD,CARAVAN) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=MOSHOOFD,y=count,fill=CARAVAN))+
  geom_bar(stat='identity')+
  geom_text(aes(label=count), vjust = -0.1, position = position_stack())   ;plot2

#plot the number of caravan insurers/subtypes 33-37
fgp = subset(caravan, MOSTYPE == 33)
fgp1 = subset(caravan, MOSTYPE == 34)
fgp2 = subset(caravan, MOSTYPE == 35)
fgp3 = subset(caravan, MOSTYPE == 36)
fgp4 = subset(caravan, MOSTYPE == 37)
fgp5 = rbind(fgp, fgp1, fgp2, fgp3, fgp4)

plot_sub1 = fgp5 %>%
  group_by(MOSTYPE,CARAVAN) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=MOSTYPE,y=count,fill=CARAVAN))+
  geom_bar(stat='identity')+
  geom_text(aes(label=count), vjust = -0.1, position = position_stack())   ;plot_sub1

#plot the number of caravan insurers/subtypes 9-13
af = subset(caravan, MOSTYPE == 9)
af1 = subset(caravan, MOSTYPE == 10)
af2 = subset(caravan, MOSTYPE == 11)
af3 = subset(caravan, MOSTYPE == 12)
af4 = subset(caravan, MOSTYPE == 13)
af5 = rbind(af, af1, af2, af3, af4)

plot_sub2 = af5 %>%
  group_by(MOSTYPE,CARAVAN) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=MOSTYPE,y=count,fill=CARAVAN))+
  geom_bar(stat='identity')+
  geom_text(aes(label=count), vjust = -0.1, position = position_stack())   ;plot_sub2

#Motorcycle and caravan policies
plot3 = caravan %>%
  select(AMOTSCO,CARAVAN) %>%
  ggplot(aes(x=as.factor(AMOTSCO),fill=CARAVAN))+
  geom_bar();plot3

#boat policies and caravan policies  
plot4 = caravan %>%
  select(APLEZIER,CARAVAN) %>%
  ggplot(aes(x=as.factor(APLEZIER),fill=CARAVAN))+
  geom_bar();plot4

#car policies and caravan policies
plot5 = caravan %>%
  select(APERSAUT,CARAVAN) %>%
  ggplot(aes(x=as.factor(APERSAUT),fill=CARAVAN))+
  geom_bar();plot5

#life policy and caravan policy
plot6 = caravan %>%
  select(ALEVEN,CARAVAN) %>%
  ggplot(aes(x=as.factor(ALEVEN),fill=CARAVAN))+
  geom_bar();plot6

#moped policy and caravan policy
plot7 = caravan %>%
  select(ABROM,CARAVAN) %>%
  ggplot(aes(x=as.factor(ABROM),fill=CARAVAN))+
  geom_bar();plot7

caravan$MAUT0.0
table(caravan$MAUT0.0)

#read in eugene's cluster solution
carcluster = read_excel("Dataset/caravanClusterSoln.xlsx")
addmargins(table(carcluster$clara_4cluster,carcluster$CARAVAN))
addmargins(table(carcluster$kmedoids_4cluster,carcluster$CARAVAN))
addmargins(table(carcluster$kmeans_4cluster,carcluster$CARAVAN))

#try to do some profiling
q= table(carcluster$kmedoids_4cluster,carcluster$MOSTYPE);q ; w = round(prop.table(q,1),3) ; w


#read in alvin's cluster solution 
p = read.csv("Dataset/clusterdata.csv")
# bb = function(x){
# ifelse(x>0,1,0)
#   }
# p$Prin1=bb(p$Prin1)
# p$Prin2=bb(p$Prin2)
# p$Prin3=bb(p$Prin3)
# p$Prin4=bb(p$Prin4)
# p$Prin5=bb(p$Prin5)
# p$Prin6=bb(p$Prin6)
# p$Prin7=bb(p$Prin7)
# p$Prin8=bb(p$Prin8)
# p$Prin9=bb(p$Prin9)
# p$Prin10=bb(p$Prin10)
# p$Prin11=bb(p$Prin11)
# p$Prin12=bb(p$Prin12)
# p$Prin13=bb(p$Prin13)
# p$Prin14=bb(p$Prin14)
# p$Prin15=bb(p$Prin15)

a2= table(p$Cluster,p$Prin6,dnn=c("cluster","Prin6"));a2 ; w = round(prop.table(a2,1),3) ; w

clusterVSpc<-aggregate(p[,c(48:62)],list(ClusterNo=p$Cluster),mean)
write.csv(clusterVSpc,file="Dataset/PCProfilingUsingMeans.csv",row.names=FALSE)

addmargins(table(p$Cluster,p$CARAVAN))

