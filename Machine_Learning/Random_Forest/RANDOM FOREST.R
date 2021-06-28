
## RANDOM FOREST

# GET DATA INTO R
# Load Library
library(readxl)

#IMPORT DATA INTO R


# set WD
setwd("C:\\Vibhaas\\Artist of Analytics\\ADSM IIM K\\Module 7 Machine Learning Supervised Learning Algorithms\\Random Forest")
getwd()

# Load Data
data <- read.csv("churn.csv")
View(data)


data <- read.csv ("clipboard", sep = "\t", header = TRUE)
names(data)
str(data)

data$Churn <- as.factor (data$Churn)


# RANDOMLY SPLITING THE DATA
library(caret)
set.seed(1234)

index <- createDataPartition(data$Churn, p = .60, list = FALSE)
train <- data[index,]
test <- data [-index,]
####################################################
# PERFORM RANDOM FOREST USING TRAIN DATA
install.packages("randomForest")
library(randomForest)
model1<- randomForest(Churn~., data = train)

print(model1)

attributes(model1)
model1$importance
###################################################

pred <- predict (model1, newdata  = test)
confusionMatrix(pred, test$Churn, positive = "TRUE")
#################################################
# TO SEE ERROR RATE 
plot(model1)
## DECIDED TO GO WITH 300 AS THE IDEAL NUMBER MTREE (AS OF NOW)
##########
#PARAMETER TUNING
# This is for identifying optimal paraemets (mtree, and mtry)
# first specifying the IVs and Dv
#stepFactor: each iteration mtry is inflated or deflated with the mentioned factor
# plot: we need a plot to identify mtry or not (whether to plot out of bag error as a function of mtry)
#ntreeTry: identified number of trees
# trace allows to insert debugging code
# improve:the relative improvement in OBB error must be by this much for the search to continue

tuneRF(train[,-17], train[,17], stepFactor = 2, plot = TRUE,ntreeTry = 300,improve = .05)
##########################
model2<- randomForest(Churn~., data = train, ntree = 300, mtry = 8)
print(model2)

plot(model2)

p1 <- predict (model2, newdata = test)

confusionMatrix(p1,test$Churn, positive = "TRUE")
################################################
# To understand variable importance
varImpPlot(model2, sort = TRUE, n.var = 10, main = 'TOP TEN VARIABLES')
importance (model2)

############################################
# USE UNDER SAMPLING

library(ROSE)
under <- ovun.sample(Churn~., data = train, method = "under", N = 580)$data
table (under$Churn)

model3<- randomForest(Churn~., data = under, ntree = 300, mtry = 4)
print(model3)

p2 <- predict (model3, newdata = test)

confusionMatrix(p2,test$Churn, positive = "TRUE")

###########################################


