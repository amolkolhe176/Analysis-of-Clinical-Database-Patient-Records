# load packages
library(data.table)
library(lubridate)
library(dplyr)
library(magrittr)
library(caret)
library(kernlab)
library(caTools)
library(rockchalk)

data$readmitted = combineLevels(data$readmitted,levs = c("<30", ">30"), newLabel = c("YES") )

#Encoding the target feature as factor
#data$readmitted = factor(data$readmitted,
                         #levels = c('<30','>30','NO'),
                         #labels = c('1', '2', '3'))



# ###########################################################################################
# #                                                                                         #
# #                                     Model Building                                      # 
# #                                                                                         #
# ###########################################################################################

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(data$readmitted, SplitRatio = 0.8)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)


library("doParallel")
library("MLmetrics")

#Register core backend, using 4 cores
cl <- makeCluster(6)
registerDoParallel(cl)


#list number of workers
getDoParWorkers()

#some parameters to control the sampling during parameter tuning and testing
ctrl <- trainControl(method="repeatedcv", number=10, repeats=2,
                     classProbs=TRUE,
                     summaryFunction = twoClassSummary, #multiClassSummary for non binary
                     allowParallel = TRUE) #default looks for parallel backend

# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
library(rpart.plot)

m.rpart <- train(readmitted ~ ., 
                 trControl = ctrl,
                 metric = "ROC", #using AUC to find best performing parameters
                 preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
                 data = training_set, 
                 method = "rpart")
m.rpart

p.rpart <- predict(m.rpart,test_set)
confusionMatrix(p.rpart,test_set$readmitted)

m.rpart.8 <- train(readmitted ~ ., 
                 trControl = ctrl,
                 metric = "ROC", #using AUC to find best performing parameters
                 preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
                 data = training_set, 
                 tuneLength=8,
                 method = "rpart")

m.rpart.8
p.rpart.8 <- predict(m.rpart.8,test_set)
confusionMatrix(p.rpart.8,test_set$readmitted)


##BAGGING - bootstrapping is used to create many training sets and simple models are trained on each and combined
##many small decision trees
library(ipred)

m.bag <- train(readmitted ~ ., 
               trControl = ctrl,
               metric = "ROC", #using AUC to find best performing parameters
               preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
               data = training_set, 
               method = "treebag")
m.bag
p.bag<- predict(m.bag,test_set)
confusionMatrix(p.bag,test_set$readmitted)

# Fitting Random Forest Classification to the Training set
#install.packages('randomForest')
library(randomForest)
library(caret)
library(pROC)
set.seed(123)

training_set
#Fitting Randon Forest
m.rf <- train(readmitted ~ ., 
              trControl = ctrl,
              metric = "ROC", #using AUC to find best performing parameters
              preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
              data = training_set, 
              method = c("rf") )

p.rf<- predict(m.rf,test_set)
confusionMatrix(p.rf,test_set$readmitted)

#compare the performance of all models trained today
rValues <- resamples(list(rpart=m.rpart, bag=m.bag, rf=m.rf))

bwplot(rValues, metric="ROC")
bwplot(rValues, metric="Sens") #Sensitvity
bwplot(rValues, metric="Spec")

#the dummyVars object is used with predict function to create new data frame of dummy variables
#excluding the response factor default (column 17)
diabetes.dummy.train <- data.frame(predict(dummyVars("~ .", data=training_set[-26], fullRank=TRUE), newdata=training_set))
diabetes.dummy.test <- data.frame(predict(dummyVars("~ .", data=test_set[-26], fullRank=TRUE), newdata=test_set))

#add the response factor to the dummy variable training and test sets 
diabetes.dummy.train<-cbind(readmitted=training_set$readmitted,diabetes.dummy.train)
diabetes.dummy.test<-cbind(readmitted=test_set$readmitted,diabetes.dummy.test)

##neural Network
#modelLookup("nnet")
m.nnet <- train(readmitted ~ ., 
                trControl = ctrl,
                metric = "ROC", #using AUC to find best performing parameters
                preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
                data = diabetes.dummy.train, 
                method = "nnet")
m.nnet
plot(m.nnet)
p.nnet <- predict(m.nnet,diabetes.dummy.test)
confusionMatrix(p.nnet,diabetes.dummy.test$readmitted)


#adaboost or adaptive boosting is an algorithm that generates wak learners 
#iteratively that learn to classify more of the examples
#install.packages("ada")
library(ada)

#boosted decision trees
#using dummy codeds because this function internally does it and its better to handle it yourself (i.e., less error prone)
m.ada <- train(readmitted ~ ., 
               trControl = ctrl,
               metric = "ROC", #using AUC to find best performing parameters
               preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
               data = diabetes.dummy.train, 
               method = "ada")
m.ada
plot(m.ada)
p.ada<- predict(m.ada,diabetes.dummy.test)
confusionMatrix(p.ada,diabetes.dummy.test$readmitted)

#compare the performance of all models trained today
rValues <- resamples(list(rpart=m.rpart, treebag = m.bag , nnet = m.nnet ,  ada = m.ada))

bwplot(rValues, metric="ROC")
bwplot(rValues, metric="Sens") #Sensitvity
bwplot(rValues, metric="Spec")

stopCluster()

