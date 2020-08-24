library("readxl")
library(caret)
library(tidyverse)
setwd("C:/Users/Alvin/Documents/GitHub/Cust-Analytics")

####################
caravan<- read.csv("Dataset_clean_v3.csv")

# get principle components output from JMP

#################################################################################
# create data partition for clustering

caravan1<-read_excel("Principlecompscore.xlsx")

# remove repeated entry
caravan1 = caravan1 %>%
  select(-MGEMLEEF)
set.seed(123)
inds = createDataPartition(caravan1$CARAVAN, p=0.5, list=FALSE,times=1)

trainset = caravan1[inds,]
nrow(trainset)/nrow(caravan1)

testset = caravan1[-inds,]
nrow(testset)/nrow(caravan1)

write.csv(trainset, "clustertrainset15PC.csv")
write.csv(testset, "clustertestset15PC.csv")

# feed csv into JMP for clustering

########################################################################
# read clustering results from JMP/R 
 
# caravanTrain<- read("PAM_train_clusterEugene.xlsx", sheet = "PAM_train_cluster")
# caravanTest<- read_excel("PAM_test_clusterEugene.xlsx", sheet = "PAM_test_cluster")

caravanTrain<- read.csv("caravanTrain.csv")
caravanTest<- read.csv("caravanTest.csv")

# cluster = rbind(caravanTrain,caravanTest)

# clustercleaned = cluster %>%
#   select(c(2:7, 29:68, 91:10))

# perform profiling for train dataset  
clustercount = caravanTrain %>%
  group_by(KMEDOID_4) %>%
  count(KMEDOID_4)

addmargins(table(caravanTrain$KMEDOID_4, caravanTrain$CARAVAN))

# perform profiling for test dataset  
clustercount = caravanTest %>%
  group_by(KMEDOID_4) %>%
  count(KMEDOID_4)

addmargins(table(caravanTest$KMEDOID_4, caravanTest$CARAVAN))

# Data visualisation
# Plot customer maintype for train data

caravanTrain %>%
  group_by(KMEDOID_4, MOSHOOFD) %>%
  select(KMEDOID_4, MOSHOOFD) %>%
  summarise(count = n(), 
            percentage = n()/nrow(caravanTrain)) %>% 
  ggplot(aes(x = KMEDOID_4, y = percentage)) +
  geom_bar(stat = 'identity') + facet_wrap(~MOSHOOFD) +
  labs(title="Customer Maintype (Train)", x = "Cluster", y = "count") 

# Plot customer maintype for test data
caravanTest %>%
  group_by(KMEDOID_4, MOSHOOFD) %>%
  select(KMEDOID_4, MOSHOOFD) %>%
  summarise(count = n(),
            percentage = n()/nrow(caravanTest)) %>%
  ggplot(aes(x = KMEDOID_4, y = percentage)) +
  geom_bar(stat = 'identity') + facet_wrap(~MOSHOOFD) +
  labs(title="Customer Maintype (Test)", x = "Cluster", y = "count") 

# Plot customer subtype for train data
caravanTrain %>%
  group_by(KMEDOID_4, MOSTYPE) %>%
  select(KMEDOID_4, MOSTYPE) %>%
  summarise(count = n(),
            percentage = n()/nrow(caravanTrain)) %>% 
  ggplot(aes(x = KMEDOID_4, y = percentage)) +
  geom_bar(stat = 'identity') + facet_wrap(~MOSTYPE) +
  labs(title="Customer Subtype (Train)", x = "Cluster", y = "count") 

# Plot customer subtype for test data
caravanTest %>%
  group_by(KMEDOID_4, MOSTYPE) %>%
  select(KMEDOID_4, MOSTYPE) %>%
  summarise(count = n(),
            percentage = n()/nrow(caravanTest)) %>% 
  ggplot(aes(x = KMEDOID_4, y = percentage)) +
  geom_bar(stat = 'identity') + facet_wrap(~MOSTYPE) +
  labs(title="Customer Subtype (Test)", x = "Cluster", y = "count") 

#################################### (To update)
# Plot houshold size
clustercleaned %>%
  group_by(Cluster, MGEMOMV) %>%
  select(Cluster, MGEMOMV) %>%
  summarise(count = n(), pert = count/nrow(clustercleaned)) %>%
  ggplot(aes(x = Cluster, y = pert)) +
  geom_bar(stat = 'identity') + facet_wrap(~MGEMOMV) +
  labs(title="Customer Subtype", x = "Cluster", y = "pert") 

# cal percentage for Prin1
Prin1 = clustercleaned %>%
  group_by(Cluster, Prin1) %>%
  filter(Prin1 > 0) %>%
  summarise(count = n()) %>%
  summarise(sum = sum(count))

Prin1total = left_join(Prin1, clustercount)
Prin1total$pert = Prin1total$sum/Prin1total$n

# cal percentage for Prin2
Prin2 = clustercleaned %>%
  group_by(Cluster, Prin2) %>%
  filter(Prin2 > 0) %>%
  summarise(count = n()) %>%
  summarise(sum = sum(count))

Prin2total = left_join(Prin2, clustercount)
Prin2total$pert = Prin2total$sum/Prin2total$n

# cal percentage for Prin3
Prin3 = clustercleaned %>%
  group_by(Cluster, Prin3) %>%
  filter(Prin3 > 0) %>%
  summarise(count = n()) %>%
  summarise(sum = sum(count))

Prin3total = left_join(Prin3, clustercount)
Prin3total$pert = Prin3total$sum/Prin3total$n

# cal percentage for Prin4
Prin4 = clustercleaned %>%
  group_by(Cluster, Prin4) %>%
  filter(Prin4 > 0) %>%
  summarise(count = n()) %>%
  summarise(sum = sum(count))

Prin4total = left_join(Prin4, clustercount)
Prin4total$pert = Prin4total$sum/Prin4total$n

# cal percentage for Prin5
Prin5 = clustercleaned %>%
  group_by(Cluster, Prin5) %>%
  filter(Prin5 > 0) %>%
  summarise(count = n()) %>%
  summarise(sum = sum(count))

Prin5total = left_join(Prin5, clustercount)
Prin5total$pert = Prin5total$sum/Prin5total$n

# cal percentage for Prin6
Prin6 = clustercleaned %>%
  group_by(Cluster, Prin6) %>%
  filter(Prin6 > 0) %>%
  summarise(count = n()) %>%
  summarise(sum = sum(count))

Prin6total = left_join(Prin6, clustercount)
Prin6total$pert = Prin6total$sum/Prin6total$n

# cal percentage for Prin7
Prin7 = clustercleaned %>%
  group_by(Cluster, Prin7) %>%
  filter(Prin7 > 0) %>%
  summarise(count = n()) %>%
  summarise(sum = sum(count))

Prin7total = left_join(Prin7, clustercount)
Prin7total$pert = Prin7total$sum/Prin7total$n

# cal percentage for Prin8
Prin8 = clustercleaned %>%
  group_by(Cluster, Prin8) %>%
  filter(Prin8 > 0) %>%
  summarise(count = n()) %>%
  summarise(sum = sum(count))

Prin8total = left_join(Prin8, clustercount)
Prin8total$pert = Prin8total$sum/Prin8total$n

# cal percentage for Prin9
Prin9 = clustercleaned %>%
  group_by(Cluster, Prin9) %>%
  filter(Prin9 > 0) %>%
  summarise(count = n()) %>%
  summarise(sum = sum(count))

Prin9total = left_join(Prin9, clustercount)
Prin9total$pert = Prin9total$sum/Prin9total$n

# cluster 1 and 2 also bought social security insurance

# cal percentage for Prin10
Prin10 = clustercleaned %>%
  group_by(Cluster, Prin10) %>%
  filter(Prin10 > 0) %>%
  summarise(count = n()) %>%
  summarise(sum = sum(count))

Prin10total = left_join(Prin10, clustercount)
Prin10total$pert = Prin10total$sum/Prin10total$n

################################################################################
# perform logistic regression on dataset

caravan<-read_excel("Principlecompscore.xlsx")

str(caravan)

# remove repeated entry
caravan1 = caravan %>%
  select(-MGEMLEEF) 

# select variables
caravan1 = caravan1[,c(1:6, 28:67, 92:106)]

# convert MOSTYPE, MOSHOOFD and CARAVAN to factor
col = caravan1[,c(3, 6, 7)]
caravan1[,c(3, 6, 7)] = lapply(col, factor)
str(caravan1)

# create train and test set for logistic regression
caravanTrain = caravan1 %>%
  filter(ORIGIN == "train")

caravanTrain = caravanTrain %>%
  select(-ORIGIN)

nrow(caravanTrain)/nrow(caravan1)

str(caravanTrain)
summary(caravanTrain)

caravanTest = caravan1 %>%
  filter(ORIGIN == "test")

caravanTest = caravanTest %>%
  select(-ORIGIN)

nrow(caravanTest)/nrow(caravan1)

# # show proportion of 0 and 1 for CARAVAN
# table(caravanTrain$CARAVAN)
# 
# # show percentage of 1 (buy caravan insurance) in dataset
# caravanTrain %>%
#   group_by(CARAVAN) %>%
#   summarise(count = n(),
#             percentage = n()/nrow(caravanTrain))
# ~6% of customer in train dataset bought caravan insurance

# use caret to downsample the train dataset
set.seed(123)
caravanTrainDN = downSample(caravanTrain, y = caravanTrain$CARAVAN, list = TRUE)[[1]]
glimpse(caravanTrainDN)

caravanTrainDN  %>%
  group_by(CARAVAN) %>%
  count(CARAVAN)

# perform logistic regression using downsampled dataset 
model = glm(CARAVAN ~.- Index, data = caravanTrainDN, family = binomial)
summary(model)

# use step function
model1 = step(model, direction = "both",  trace = T)
summary(model1)

# alternatively use caret to upsample the train dataset
set.seed(123)
caravanTrainUP = upSample(caravanTrain, y = caravanTrain$CARAVAN, list = TRUE)[[1]]
glimpse(caravanTrainUP)

caravanTrainUP  %>%
  group_by(CARAVAN) %>%
  count(CARAVAN)

# perform logistic regression using upsampled dataset (34min)
set.seed(123)
model = glm(CARAVAN ~.- Index, data = caravanTrainUP, family = binomial)
summary(model)

# use step function
model1 = step(model, direction = "both",  trace = T)
summary(model1)

saveRDS(model1, file = "caravanlogregupsample.rds")

# read model
model1 <- readRDS("caravanlogregupsample.rds") 

# test it on train set
trainPredict = predict(model1, newdata = caravanTrain, type = "response")
responseTrain = data.frame(trainPredict)

# show confusion matrix
p_class = ifelse(trainPredict > 0.5,1,0)

matrix_table = table(caravanTrain$CARAVAN, p_class)
matrix_table

accuracy = sum(diag(matrix_table))/sum(matrix_table)
round(accuracy, 2)

# test it on test set
testPredict = predict(model1, newdata = caravanTest, type = "response")

# show confusion matrix
p_class = ifelse(testPredict > 0.5,1,0)

matrix_table = table(caravanTest$CARAVAN, p_class)
matrix_table

accuracy = sum(diag(matrix_table))/sum(matrix_table)
round(accuracy, 2)

# append probability to original dataset
caravanTrain$probability = trainPredict
caravanTest$probability = testPredict

caravanTrain$score = trainPredict * 1000
caravanTest$score = testPredict * 1000

rocTrain=roc(caravanTrain,CARAVAN,probability,plot=TRUE,grid=TRUE,legacy.axes = TRUE,smooth=TRUE)
lines(rocTrain, col="red", type='l')
text(0.4, 0.43, labels=sprintf("AUC: %0.3f", auc(rocTrain)), col="red")

rocTest=roc(caravanTest,CARAVAN,probability,plot=TRUE,add=TRUE,smooth=TRUE)
lines(rocTest, col="blue", type='l')
text(0.4, 0.35, labels=sprintf("AUC: %0.3f", auc(rocTest)), col="blue")

# rbind train and test set 
overall = rbind(caravanTrain,caravanTest)

summary(overall)

# original dataset is named "overall"

# show overall distribution of scores
overall %>%
  ggplot(aes(x = score)) +
    geom_histogram(binwidth=100) 

# read csv containing cluster information
caravanTrainCluster <- read.csv("caravanTrain.csv")
caravanTestCluster  <- read.csv("caravanTest.csv")

clustercountTrain = caravanTrainCluster %>%
  group_by(KMEDOID_4) %>%
  count(KMEDOID_4)

addmargins(table(caravanTrainCluster$KMEDOID_4, caravanTrainCluster$CARAVAN))

# perform profiling for test dataset  
clustercountTest = caravanTestCluster %>%
  group_by(KMEDOID_4) %>%
  count(KMEDOID_4)

addmargins(table(caravanTestCluster$KMEDOID_4, caravanTestCluster$CARAVAN))

# show proportion of 0 and 1 for CARAVAN
caravanTrainCluster %>%
  group_by(KMEDOID_4, CARAVAN) %>%
  summarise(count = n(), percentage = n()/nrow(caravanTrainCluster))

str(caravanTestCluster$KMEDOID_4)
caravanTestCluster$KMEDOID_4 = factor(caravanTestCluster$KMEDOID_4)

# recode cluster no (KMEDOID_4) in caravanTest (recode cluster 1 to 2 and recode cluster 2 to 1) 
caravanTestCluster = caravanTestCluster%>%
  mutate(KMEDOID_4new = recode_factor(KMEDOID_4, "2" = "5"))
caravanTestCluster = caravanTestCluster%>%
  mutate(KMEDOID_4new = recode_factor(KMEDOID_4new, "1" = "2"))
caravanTestCluster = caravanTestCluster%>%
  mutate(KMEDOID_4new = recode_factor(KMEDOID_4new, "5" = "1"))

caravanTestCluster$KMEDOID_4 = caravanTestCluster$KMEDOID_4new

caravanTestCluster = caravanTestCluster%>%
  select(-KMEDOID_4new)

# select only Index and kmedoid variables from cluster dataset 
caravanTrainCluster1 = caravanTrainCluster%>%
  select(c(Index, KMEDOID_4))

caravanTestCluster1 = caravanTestCluster%>%
  select(c(Index, KMEDOID_4))

CaravanOverallCluster = rbind(caravanTrainCluster1,caravanTestCluster1)

# append "overall" dataset to cluster dataset via join function
overall2 = full_join(overall, CaravanOverallCluster, by = "Index")

# plot distribution
overall2 %>%
  ggplot(aes(x = KMEDOID_4, y = score)) +
  geom_boxplot() 

# cluster 1 and 2 have the highest median
# tells you range of score

# split by cluster

cluster1 = overall2 %>%
  filter(KMEDOID_4 == 1)

cluster2 = overall2 %>%
  filter(KMEDOID_4 == 2)

cluster3 = overall2 %>%
  filter(KMEDOID_4 == 3)

cluster4 = overall2 %>%
  filter(KMEDOID_4 == 4)


caravanTrain %>% 
  group_by(CARAVAN) %>% 
  summarise(tb = mean(score)) %>% 
  ungroup() -> mean_score_train

caravanTrain %>% 
  ggplot(aes(score, color = CARAVAN, fill = CARAVAN)) + 
  geom_density(alpha = 0.3) + 
  geom_vline(aes(xintercept = mean_score_train$tb[1]), linetype = "dashed", color = "red") + 
  geom_vline(aes(xintercept = mean_score_train$tb[2]), linetype = "dashed", color = "blue") + 
  geom_text(aes(x = 400 - 15, y = 0.0042, label = mean_score_train$tb[1] %>% round(0)), color = "red", size = 4) + 
  geom_text(aes(x = 565, y = 0.0042, label = mean_score_train$tb[2] %>% round(0)), color = "blue", size = 4) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.2, 0.8)) + 
  labs(x = NULL, y = NULL, title = "Figure 1: Scorecard Distribution for Train Data", 
       subtitle = "The scorecard point is a numeric expression measuring \n propensity to purchase Caravan Insurance.")

caravanTest %>% 
  group_by(CARAVAN) %>% 
  summarise(tb = mean(score)) %>% 
  ungroup() -> mean_score_train

caravanTest %>% 
  ggplot(aes(score, color = CARAVAN, fill = CARAVAN)) + 
  geom_density(alpha = 0.3) + 
  geom_vline(aes(xintercept = mean_score_train$tb[1]), linetype = "dashed", color = "red") + 
  geom_vline(aes(xintercept = mean_score_train$tb[2]), linetype = "dashed", color = "blue") + 
  geom_text(aes(x = 400 - 15, y = 0.0042, label = mean_score_train$tb[1] %>% round(0)), color = "red", size = 4) + 
  geom_text(aes(x = 565, y = 0.0042, label = mean_score_train$tb[2] %>% round(0)), color = "blue", size = 4) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.2, 0.8)) + 
  labs(x = NULL, y = NULL, title = "Figure 2: Scorecard Distribution for Test Data", 
       subtitle = "The scorecard point is a numeric expression measuring \n propensity to purchase Caravan Insurance.")

library(rbin)

# perform binning for overall
bins0 = rbin_manual(overall2, CARAVAN, score, c(100, 200, 300, 400, 500, 600, 680, 740, 830))
binsOverall = data.frame(bins0[[1]])

# calculate lift for overall
overallGoodrate0 = binsOverall$good_cum_count[nrow(binsOverall)]/ nrow(overall2)
binsOverall$lift = binsOverall$good_rate/overallGoodrate

# create variable score interval for overall
binsOverall$score_interval = c(50, 150, 250, 350, 450, 550, 640, 720, 785, 915)

# plot lift for overall
binsOverall %>%
  ggplot(aes(x = score_interval, y = lift)) +
  geom_point(shape=23, fill="blue", size=3) + geom_line() + geom_hline(yintercept = 1, col = "red") + 
  ggtitle("Lift Chart for Overall Dataset") + ylab("Lift") + 
  xlab("Score Interval") + geom_text(aes(0,1,label = "No Model", vjust = -1))

# perform binning for cluster1 (our target group)
bins1 = rbin_manual(cluster1, CARAVAN, score, c(100, 200, 300, 400, 500, 580, 660, 740, 820))
binsCluster1 = data.frame(bins1[[1]])

# calculate lift for cluster1
overallGoodrate = binsCluster1$good_cum_count[nrow(binsCluster1)]/ nrow(cluster1)
binsCluster1$lift = binsCluster1$good_rate/overallGoodrate;binsCluster1

# create variable score interval for cluster1
binsCluster1$score_interval = c(50, 150, 250, 350, 450, 540, 620, 700, 790, 920)

# plot lift for cluster 1
binsCluster1 %>%
  ggplot(aes(x = score_interval, y = lift)) +
  geom_point(shape=23, fill="blue", size=3) + geom_line() + geom_hline(yintercept = 1, col = "red") + 
  ggtitle("Lift Chart for Cluster 1") + ylab("Lift") + 
  xlab("Score Interval") + geom_text(aes(0,1,label = "No Model", vjust = -1))
  
# perform binning for cluster2 (our target group)
bins2 = rbin_manual(cluster2, CARAVAN, score, c(150, 300, 400, 500, 600, 680, 790))
binsCluster2 = data.frame(bins2[[1]])

# calculate lift for cluster2 
overallGoodrate2 = binsCluster2$good_cum_count[nrow(binsCluster2)]/ nrow(cluster2)
binsCluster2$lift = binsCluster2$good_rate/overallGoodrate2;binsCluster2

# create variable score interval for cluster2
binsCluster2$score_interval = c(75, 225, 350, 450, 550, 640, 735, 895)

# plot lift for cluster 2
binsCluster2 %>%
  ggplot(aes(x = score_interval, y = lift)) +
  geom_point(shape=23, fill="blue", size=3) + geom_line() + geom_hline(yintercept = 1, col = "red") + 
  ggtitle("Lift Chart for Cluster 2") + ylab("Lift") + 
  xlab("Score Interval") + geom_text(aes(0,1,label = "No Model", vjust = -1))

# perform binning (cluster3)
bins3 = rbin_manual(cluster3, CARAVAN, score, c(100,200,300,400, 650))
binsCluster3 = data.frame(bins3[[1]])

# calculate lift for cluster3 
overallGoodrate3 = binsCluster3$good_cum_count[nrow(binsCluster3)]/ nrow(cluster3)
binsCluster3$lift = binsCluster3$good_rate/overallGoodrate3;binsCluster3

# perform binning (cluster4)
bins4 = rbin_manual(cluster4, CARAVAN, score, c(150, 180, 210, 240))
binsCluster4 = data.frame(bins4[[1]])

# calculate lift for cluster4 
overallGoodrate4 = binsCluster4$good_cum_count[nrow(binsCluster4)]/ nrow(cluster4)
binsCluster4$lift = binsCluster4$good_rate/overallGoodrate4;binsCluster4

####################################################################################
# alternative method to generate scorecard (did not take into account cluster)
breaks <- c(0, 100,200,300,400,500,600,700,800,900, 1000)
# specify interval/bin labels
tags <- c("[0-100)","[100-200)", "[300-400)", "[400-500)", "[500-600)", "[600-700)","[700-800)", "[800-900)","[900-1000)", "[1000-10000)")
# bucketing values into bins
group_tags <- cut(cluster1$score, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
range(cluster1$score)
scorecardcluster1 = data.frame(summary(group_tags))

#####################################################################
# alternative method - use scorecard function (did not take into account cluster)

library(scorecard)

caravan<-read_excel("Principlecompscore.xlsx")

str(caravan)

# remove repeated entry
caravan1 = caravan %>%
  select(-MGEMLEEF) 

caravan1 = caravan1[,c(1:6, 28:67, 92:106)]

# convert MOSTYPE, MOSHOOFD and CARAVAN to factor
col = caravan1[,c(3, 6, 7)]
caravan1[,c(3, 6, 7)] = lapply(col, factor)
str(caravan1)

# create train and test set for logistic regression
caravanTrain = caravan1 %>%
  filter(ORIGIN == "train")

caravanTrain = caravanTrain %>%
  select(-ORIGIN)

nrow(caravanTrain)/nrow(caravan1)

str(caravanTrain)
summary(caravanTrain)

caravanTest = caravan1 %>%
  filter(ORIGIN == "test")

caravanTest = caravanTest %>%
  select(-ORIGIN)

nrow(caravanTest)/nrow(caravan1)

# filter variable via missing rate, iv, identical value rate
dt_s = var_filter(caravan1, y="CARAVAN")

# woe binning
bins = woebin(dt_s, y="CARAVAN")

# binning adjustment
# breaks_adj = woebin_adj(dt_s, "CARAVAN", bins) 
# alternative
# Generates optimal binning for numerical, factor and categorical variables: 
bins_var <- woebin(caravan1, y = "CARAVAN", no_cores = 20, positive = "CARAVAN|1")

# Creates a data frame of binned variables for Logistic Regression
# converting train and test into woe values
train_woe <- woebin_ply(caravanTrain, bins_var)
test_woe = woebin_ply(caravanTest, bins_var)

# Logistic Regression
model2 <- glm(CARAVAN ~ ., family = binomial, data = train_woe)

# Show results: 
summary(model2)

# Apply step function
model_step <- step(model2, direction = "both", trace = FALSE)
m2 = eval(model_step$call)

train_pred = predict(m2, train_woe, type='response')
test_pred = predict(m2, test_woe, type='response')

# show confusion matrix (train)
p_class = ifelse(train_pred > 0.4,1,0)

matrix_table = table(caravanTrain$CARAVAN, p_class)
matrix_table

accuracy = sum(diag(matrix_table))/sum(matrix_table)
round(accuracy, 2)

# show confusion matrix (test)
p_class = ifelse(test_pred > 0.4,1,0)

matrix_table = table(caravanTest$CARAVAN, p_class)
matrix_table

accuracy = sum(diag(matrix_table))/sum(matrix_table)
round(accuracy, 2)

# score 
card = scorecard(bins_var, m2)
# credit score
train_score = scorecard_ply(caravanTrain, card, print_step=0)
test_score = scorecard_ply(caravanTest, card, print_step=0)

# psi
perf_psi(
  score = list(caravanTrain = train_score, caravanTest = test_score),
  label = list(caravanTrain= caravanTrain$CARAVAN, caravanTest = caravanTest$CARAVAN)
)
