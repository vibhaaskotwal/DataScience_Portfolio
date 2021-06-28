# Load Library
library(readxl)


        
data <- read.csv ("clipboard", sep = "\t", header = TRUE)
library(caret)
names(data)
str(data)
data$Churn <- as.factor (data$Churn)

set.seed(1234)

index <-createDataPartition(data$Churn, p  = .60, list = FALSE)
train <- data[index,]
test <- data [-index,]
############################
# ADDING REQUIRED LIBRARIES 
############################
install.packages ("rpart")
install.packages ("rpart.plot")
library(rpart)
library(rpart.plot)
#########################

model <- rpart (Churn~., data = train)
print(model)
rpart.plot (model, extra = 3)
print(model)

p <- predict (model, newdata = test, type = 'class')
p
p1 <- predict (model, newdata = test, type = 'prob')
View(p1)
confusionMatrix(p, test$Churn, positive = "TRUE")
prop.table (table (train$Churn))
#########PLOT ROC CURVE
library(pROC)
par (pty = "s")
roc(test$Churn, p1[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE, xlab = "FALSE POSITIVE PERCENTAGE",
    ylab = "TRUE POSITIVE PERCENTAGE", col = "#2c7fb8", lwd = 4, print.auc = TRUE)


## USING OVER SAMPLING
# CHECK DATA IMBALANCE ISSUE
table (train$Churn)
#PERFORM A MODEL WITH OVERSAMPLED DATA
library(ROSE)
1710*2
over<- ovun.sample(Churn~., data = train, method = "over", N = 3420)$data
table (over$Churn)

model1 <- rpart (Churn~., data = over)
rpart.plot (model1, extra = 3)

p3 <- predict (model1, newdata = test, type = 'class')
View(p)
confusionMatrix(p3, test$Churn, positive = "TRUE")

## USING UNDER SAMPLING 
table (train$Churn)
290*2
set.seed (1234)
under <- ovun.sample(Churn~., data = train, method = "under", N = 580)$data
table (under$Churn)

model2 <- rpart (Churn~., data = under)
rpart.plot (model2, extra = 2)
library(caret)

p4 <- predict (model2, newdata = test, type = 'class')
View(p2)
confusionMatrix(p4, test$Churn, positive = "TRUE")

#####################
#USING BOTH FUNCTION
both <- ovun.sample(Churn~., data = train, method = "both", p = .50, seed = 1234, N = 2000)$data
table (both$Churn)
model3 <- rpart (Churn~., data = both)
rpart.plot (model3, extra = 3)

p5 <- predict (model3, newdata = test, type = 'class')
View(p3)
confusionMatrix(p5, test$Churn, positive = "TRUE")

### by comparing the sensititivty across random samples, we found that
# it is higher at over sampled data
# So we will use over sampled data to do prediction
# WE WILL DRAW ROC USING OVER SAMPLED DATA

p6 <- predict (model1, newdata = test, type = 'prob')

roc(test$Churn, p6[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE, xlab = "FALSE POSITIVE PERCENTAGE",
    ylab = "TRUE POSITIVE PERCENTAGE", col = "#2c7fb8", lwd = 4, print.auc = TRUE)


## TO UNDERSTAND THE IMPORTANT VARIABLES
model1$variable.importance
rpart.plot(model1)

# DECISION TREE USING CTREE FUNCTION
install.packages ("party")

### FOR INFORMATION#### NOT NEEDED
library(party)
m <- ctree (Churn ~., data = under)
plot (m)

table (train$Churn)

