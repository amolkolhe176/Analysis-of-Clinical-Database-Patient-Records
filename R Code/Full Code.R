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
rValues <- resamples(list(rpart=m.rpart, treebag = m.bag , nnet = m.nnet , ada = m.ada))
bwplot(rValues, metric="ROC")
bwplot(rValues, metric="Sens") #Sensitvity
bwplot(rValues, metric="Spec")
stopCluster()
##################################################################################
# load packages
library(data.table)
library(lubridate)
library(dplyr)
library(magrittr)
library(caret)
library(klaR)
data = readRDS("Diabetic_cleaned.rds")
lda.data<- data
str(lda.data)
# excluding irrelevant columns (patient_nbr,examide) that have too many levels (diag_1,diag_2,diag_3)
lda.data$examide<-NULL
lda.data$patient_nbr<-NULL
lda.data$diag_1<-NULL
lda.data$diag_2<-NULL
#converting int variables to numeric
lda.data$time_in_hospital<-as.numeric(lda.data$time_in_hospital)
lda.data$num_lab_procedures<-as.numeric(lda.data$num_lab_procedures)
lda.data$num_procedures<-as.numeric(lda.data$num_procedures)
lda.data$num_medications<-as.numeric(lda.data$num_medications)
lda.data$number_outpatient<-as.numeric(lda.data$number_outpatient)
lda.data$number_inpatient<-as.numeric(lda.data$number_inpatient)
lda.data$number_emergency<-as.numeric(lda.data$number_emergency)
lda.data$number_diagnoses<-as.numeric(lda.data$number_diagnoses)
#converting factors variable into dummies
lda.data.dummies<-dummyVars( ~ .-readmitted,data=lda.data,fullRank=T)
dummied.lda.data <- as.data.frame(predict(lda.data.dummies,newdata=lda.data))
dummied.lda.data$readmitted<-lda.data$readmitted
#print(dummiedData)
#Splitting the data to train and test set
split = sample.split(lda.data$readmitted, SplitRatio = 0.8)
lda_training_set = subset(lda.data, split == TRUE)
lda_test_set = subset(lda.data, split == FALSE)
#table(training_set$readmitted)
ctrl <- trainControl(method = "cv", number=10, summaryFunction=multiClassSummary,classProbs=F,savePredictions=T) #saving
predictions from each resample fold
set.seed(199)
#fit the LDA model on lda_training_set
model.lda <- train(readmitted ~ ., data=lda_training_set , preProc = c("center", "scale", "BoxCox", "nzv"),method="lda",
                   metric="Accuracy", trControl=ctrl)
lda_predictions<-predict(model.lda,test_set)
#show confusion matrix of the results
table(predictions,lda_test_set$readmitted)
# show detailed confusion matrix of the results
confusionMatrix(lda_predictions,lda_test_set$readmitted)
##############################################################################################
lda.data_smote<- data
#converting int variables to numeric
lda.data_smote$time_in_hospital<-as.numeric(lda.data_smote$time_in_hospital)
lda.data_smote$num_lab_procedures<-as.numeric(lda.data_smote$num_lab_procedures)
lda.data_smote$num_procedures<-as.numeric(lda.data_smote$num_procedures)
lda.data_smote$num_medications<-as.numeric(lda.data_smote$num_medications)
lda.data_smote$number_outpatient<-as.numeric(lda.data_smote$number_outpatient)
lda.data_smote$number_inpatient<-as.numeric(lda.data_smote$number_inpatient)
lda.data_smote$number_emergency<-as.numeric(lda.data_smote$number_emergency)
lda.data_smote$number_diagnoses<-as.numeric(lda.data_smote$number_diagnoses)
# removing irrelevant columns (patient_nbr,examide) and columns with too many levels (diag_1,diag_2)
lda.data_smote$patient_nbr<-NULL
lda.data_smote$diag_1<-NULL
lda.data_smote$diag_2<-NULL
lda.data_smote$examide<-NULL
#splitting the data to training set and testing set
split = sample.split(dd$readmitted, SplitRatio = 0.8)
training_set_lda_smote = subset(dd, split == TRUE)
test_set_lda_smote = subset(dd, split == FALSE)
table(training_set_lda_smote$readmitted)
#filtering rows who belong to either "<30" class or ">30" class
filtered_first_pair_of_classes_data<-filter(training_set_lda_smote , readmitted == "<30" | readmitted == ">30")
#drop levels if there are levels who don't appear in the "filtered_first_2_classes_data" dataset
filtered_first_pair_of_classes_data<-droplevels(filtered_first_pair_of_classes_data)
# performing SMOTE on first pair of classes ("<30" and ">30")
balanced_first_pair_of_classes_data <- SMOTE(readmitted ~ ., data = filtered_first_pair_of_classes_data,perc.over =
                                               600,perc.under=100)
#filtering rows who belong to either "<30" class or "NO" class
filtered_second_pair_of_classes_data<-filter(training_set_lda_smote , readmitted == "<30" | readmitted == "NO")
#drop levels if there are levels who don't appear in the filtered_data2 dataset
filtered_second_pair_of_classes_data<-droplevels(filtered_second_pair_of_classes_data)
# performing SMOTE on second pair of classes ("<30" and "NO")
balanced_second_pair_of_classes_data <- SMOTE(readmitted ~ ., data = filtered_second_pair_of_classes_data,perc.over =
                                                600,perc.under=100)
#filter all the records that belong to class "NO" from the balanced dataset
No_class_data<-filter(balanced_second_pair_of_classes_data, readmitted == "NO")
#drop unused levels from "No_class_data"
No_class_data<-droplevels(No_class_data)
#combine the first balanced dataset with the balanced NO class to get dataset with 3 balanced classes
balanced_3_classes_data <- rbind(balanced_first_pair_of_classes_data, No_class_data)
table(balanced_3_classes_data$readmitted)
#extracting sample of 9000 records equally balanced between 3 classes
twocls<-rbind(total[1:3000,], total[36000:39000,])
threecls<-rbind(twocls, total[90000:93000,])
table(threecls$readmitted)
#convert the balanced data into dummies
balanced.data.dummies<-dummyVars( ~ .-readmitted,data=balanced_3_classes_data,fullRank=T)
dummied.balanced.data <- as.data.frame(predict(balanced.data.dummies,newdata=balanced_3_classes_data))
dummied.balanced.data$readmitted<-balanced_3_classes_data $readmitted
#convert the test set data into dummies
test.dummies<-dummyVars( ~ .-readmitted,data=test_set_lda_smote,fullRank=T)
lda.smote.test.dummied.data <- as.data.frame(predict(test.dummies,newdata=test_set_lda_smote))
lda.smote.test.dummied.data$readmitted<-test_set_lda_smote$readmitted
##train the LDA model to run on SMOTE balanced training data
ctrl <- trainControl(method = "cv", number=10, summaryFunction=multiClassSummary,classProbs=F,savePredictions=T) #saving
predictions from each resample fold
set.seed(199)
LC.lda.balanced <- train(readmitted ~ ., data=dummied.balanced.data , preProc = c("center", "scale", "BoxCox",
                                                                                  "nzv"),method="lda", metric="Accuracy", trControl=ctrl)
#predict the results on the test data
predictions.lda.balanced<-predict(LC.lda.balanced ,lda.smote.test.dummied.data)
#show confusion matrix of the results
table(predictions.lda.balanced,lda.smote.test.dummied.data$readmitted)
#show detailed confusion matrix of the results
confusionMatrix(predictions.lda.balanced,lda.smote.test.dummied.data$readmitted)
##############################################################################################
# load packages
library(data.table)
library(lubridate)
library(dplyr)
library(magrittr)
library(caret)
library(klaR)
library(e1071) # load the library
#install.packages('caTools')
library(caTools)
library(MLmetrics)
# # Setting up working directory
# getwd()
setwd("C:/Users/Yana1801/Documents/mis 620/diabetic project")
#read the data
data=readRDS("Diabetic_data_cleaned.rds")
data_numeric_naive_bayes<-data
#delete irelevant columns (patient_nbr) and columns with too many levels (diag_3)
data_numeric_naive_bayes$diag_3<- NULL
data_numeric_naive_bayes$examide<- NULL
data_numeric_naive_bayes$discharge_disposition_id<-NULL
data_numeric_naive_bayes$admission_source_id<-NULL
data_numeric_naive_bayes$patient_nbr<-NULL
set.seed(123)
split = sample.split(data_numeric_naive_bayes$readmitted, SplitRatio = 0.8)
training_set_bayes_numeric = subset(data_numeric_naive_bayes, split == TRUE)
test_set_bayes_numeric = subset(data_numeric_naive_bayes, split == FALSE)
# ###########################################################################################
# Splitting the dataset into the Training set and Test set
#--------------------------------------Balancing the data -------------------------------------------
library(DMwR)
#filtering rows who belong to either "<30" class or ">30" class
filtered_first_pair_of_classes_data<-filter(training_set_bayes_numeric , readmitted == "<30" | readmitted == ">30")
#drop levels if there are levels who don't appear in the "filtered_first_2_classes_data" dataset
filtered_first_pair_of_classes_data<-droplevels(filtered_first_pair_of_classes_data)
# performing SMOTE on first pair of classes ("<30" and ">30")
balanced_first_pair_of_classes_data <- SMOTE(readmitted ~ ., data = filtered_first_pair_of_classes_data,perc.over =
                                               600,perc.under=100)
#filtering rows who belong to either "<30" class or "NO" class
filtered_second_pair_of_classes_data<-filter(training_set_bayes_numeric , readmitted == "<30" | readmitted == "NO")
#drop levels if there are levels who don't appear in the filtered_data2 dataset
filtered_second_pair_of_classes_data<-droplevels(filtered_second_pair_of_classes_data)
# performing SMOTE on second pair of classes ("<30" and "NO")
balanced_second_pair_of_classes_data <- SMOTE(readmitted ~ ., data = filtered_second_pair_of_classes_data,perc.over =
                                                600,perc.under=100)
#filter all the records that belong to class "NO" from the balanced dataset
No_class_data<-filter(balanced_second_pair_of_classes_data, readmitted == "NO")
#drop unused levels from "No_class_data"
No_class_data<-droplevels(No_class_data)
#combine the first balanced dataset with the balanced NO class to get dataset with 3 balanced classes
balanced_3_classes_data <- rbind(balanced_first_pair_of_classes_data, No_class_data)
table(balanced_3_classes_data$readmitted)
twocls<-rbind(total[1:3000,], total[36000:39000,])
threecls<-rbind(twocls, total[90000:93000,])
table(threecls$readmitted)
############################################################################################
#numerical Naive Bayes--------------------------------------------#
set.seed(9560)
ctrl <- trainControl(method = "cv", number=10, summaryFunction=multiClassSummary,savePredictions=T) #saving predictions
from each resample fold
nB_mode_train_numerical <- train(readmitted ~., data = balanced_3_classes_data, method="nb", metric="Accuracy",preProc =
                                   c("center", "nzv","scale","BoxCox"),tuneGrid =data.frame(usekernel=TRUE,fL=1,adjust=1))
warnings()
test_set$discharge_disposition_id<- NULL
numeric_bayes_predictions<-predict(nB_mode_train_numerical,test_set_bayes_numeric)
table(numeric_bayes_predictions,test_set_bayes_numeric$readmitted)
confusionMatrix(numeric_bayes_predictions,test_set_bayes_numeric$readmitted)
#############################################################################################
# load packages
library(data.table)
library(lubridate)
library(dplyr)
library(magrittr)
######################################################################################
data<-readRDS("Diabetic_cleaned.rds")
data_categorical_naive_bayes<-data
#remove irrelevant columns (examide,patient_nbr) and columns with too many levels (diag_3)
data_categorical_naive_bayes$diag_1<- NULL
data_categorical_naive_bayes$diag_2<- NULL
data_categorical_naive_bayes$discharge_disposition_id<-NULL
data_categorical_naive_bayes$admission_source_id<-NULL
#discretize the numeric variables (convert numeric variables into factors by using "cut")
time_in_hospital_categories_number=2
data_categorical_naive_bayes$time_in_hospital =
  as.factor(cut(data_categorical_naive_bayes$time_in_hospital,time_in_hospital_categories_number))
lab_procedures_categories_number=5
data_categorical_naive_bayes$num_lab_procedures =
  as.factor(cut(data_categorical_naive_bayes$num_lab_procedures,lab_procedures_categories_number))
outpatient_categories_number=3
data_categorical_naive_bayes$number_outpatient <-
  as.factor(cut(data_categorical_naive_bayes$number_outpatient,outpatient_categories_number))
inpatient_categories_number=2
data_categorical_naive_bayes$number_inpatient <-
  as.factor(cut(data_categorical_naive_bayes$number_inpatient,inpatient_categories_number))
emergency_categories_number=3
data_categorical_naive_bayes$number_emergency <-
  as.factor(cut(data_categorical_naive_bayes$number_emergency,emergency_categories_number))
diagnoses_categories_number=2
data_categorical_naive_bayes$number_diagnoses <-
  as.factor(cut(data_categorical_naive_bayes$number_diagnoses,diagnoses_categories_number))
procedures_categories_number=2
data_categorical_naive_bayes$num_procedures <-as.factor(cut(data_categorical_naive_bayes$num_procedures
                                                            ,procedures_categories_number))
medications_categories_number=3
data_categorical_naive_bayes$num_medications <-as.factor(cut(data_categorical_naive_bayes$num_medications,
                                                             medications_categories_number))
set.seed(123)
# Splitting the dataset into the Training set and Test set
split = sample.split(data_categorical_naive_bayes$readmitted, SplitRatio = 0.8)
training_set_bayes_categorical = subset(data_categorical_naive_bayes, split == TRUE)
test_set_bayes_categorical = subset(data_categorical_naive_bayes, split == FALSE)
#--------------------------------------Balancing the data -------------------------------------------
library(DMwR)
#filtering rows who belong to either "<30" class or ">30" class
filtered_first_pair_of_classes_data<-filter(training_set_lda_smote , readmitted == "<30" | readmitted == ">30")
#drop levels if there are levels who don't appear in the "filtered_first_2_classes_data" dataset
filtered_first_pair_of_classes_data<-droplevels(filtered_first_pair_of_classes_data)
# performing SMOTE on first pair of classes ("<30" and ">30")
balanced_first_pair_of_classes_data <- SMOTE(readmitted ~ ., data = filtered_first_pair_of_classes_data,perc.over =
                                               600,perc.under=100)
#filtering rows who belong to either "<30" class or "NO" class
filtered_second_pair_of_classes_data<-filter(training_set_lda_smote , readmitted == "<30" | readmitted == "NO")
#drop levels if there are levels who don't appear in the filtered_data2 dataset
filtered_second_pair_of_classes_data<-droplevels(filtered_second_pair_of_classes_data)
# performing SMOTE on second pair of classes ("<30" and "NO")
balanced_second_pair_of_classes_data <- SMOTE(readmitted ~ ., data = filtered_second_pair_of_classes_data,perc.over =
                                                600,perc.under=100) 
#filter all the records that belong to class "NO" from the balanced dataset
No_class_data<-filter(balanced_second_pair_of_classes_data, readmitted == "NO")
#drop unused levels from "No_class_data"
No_class_data<-droplevels(No_class_data)
#combine the first balanced dataset with the balanced NO class to get dataset with 3 balanced classes
balanced_3_classes_data <- rbind(balanced_first_pair_of_classes_data, No_class_data)
table(balanced_3_classes_data$readmitted)
#twocls<-rbind(total[1:3000,], total[36000:39000,])
#threecls<-rbind(twocls, total[90000:93000,])
#table(threecls$readmitted)
############################################################################################
set.seed(9560)
ctrl <- trainControl(method = "cv", number=10, summaryFunction=multiClassSummary,savePredictions=T) #saving predictions
from each resample fold
#fitting naive bayes model to SMOTE balanced training data
nB_mode_train_categorical <- train(readmitted ~., data = balanced_3_classes_data, method="nb", metric="Accuracy",preProc =
                                     c("center", "nzv","scale"),tuneGrid =data.frame(usekernel=TRUE,fL=1,adjust=1))
#predicts the class on the testing set
categorical_bayes_predictions<-predict(nB_mode_train_categorical,test_set_bayes_categorical)
#shows the confusion matrix
table(categorical_bayes_predictions,test_set_bayes_categorical$readmitted)
#shows detailed confusion matrix
confusionMatrix(categorical_bayes_predictions,test_set_bayes_categorical$readmitted)
#############################################################################################
library(ggplot2)
library(dplyr)
data = readRDS("Diabetic_cleaned.rds")
# class distribution
ggplot(data, aes(x="", fill = readmitted)) +
  geom_bar(width=0.3) + xlab("") +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  ggtitle("Class distribution")
# gender distribution
ggplot(data,aes(x=gender, fill=readmitted)) +
  geom_bar() +
  xlab("") + labs(fill="Readmitted") +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  ggtitle("Gender distribution")
# age distribution
ggplot(data,aes(x=age, fill=readmitted)) +
  geom_bar() +
  xlab("Age bins") + labs(fill="Readmitted") +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  ggtitle("Age distribution")
# race distribution
ggplot(data,aes(x=race, fill=readmitted)) +
  geom_bar() +
  xlab("") + labs(fill="Readmitted") +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  ggtitle("Race distribution")
# time in hospital
ggplot(data,aes(x=time_in_hospital, fill=readmitted)) +
  geom_bar() +
  xlab("Days in hospital") +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  ggtitle("Distribution of days in hospital")
# number of medications
ggplot(data %>% filter(num_medications < 50),aes(x=num_medications, fill=readmitted)) + ##
  geom_bar() +
  xlab("Number of medications") +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  ggtitle("Number of medications")
# number of procedures
ggplot(data,aes(x=num_procedures, fill=readmitted)) + ##
  geom_bar() +
  xlab("Number of procedures") +
  ggtitle("Number of procedures") +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73"))
# number of lab procedures
ggplot(data %>% filter(num_lab_procedures < 100),aes(x=num_lab_procedures, fill=readmitted)) + ##
  geom_bar() +
  xlab("Number of lab procedures") +
  ggtitle("Number of lab procedures") +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73"))
# number of outpatient visits
ggplot(data %>% filter(number_outpatient < 5),aes(x=number_outpatient, fill=readmitted)) +
  geom_bar() +
  xlab("Number of outpatient visits") +
  ggtitle("Number of outpatient visits") +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  
  
  # number of inpatient visits
  ggplot(data %>% filter(number_inpatient < 5),aes(x=number_inpatient, fill=readmitted)) +
  geom_bar() +
  xlab("Number of inpatient visits") +
  ggtitle("Number of inpatient visits") +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73"))
# number of emergency visits
ggplot(data %>% filter(number_emergency < 5),aes(x=number_emergency, fill=readmitted)) +
  geom_bar() +
  xlab("Number of emergency visits") +
  ggtitle("Number of emergency visits") +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73"))
# diagnosises:
# primary diagnosis
ggplot(data,aes(x=diag_1, fill=readmitted)) +
  geom_bar() +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  xlab("Primary diagnosis") +
  ggtitle("Primary diagnosis")
# secondary
ggplot(data,aes(x=diag_2, fill=readmitted)) +
  geom_bar() +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  xlab("Secondary diagnosis") +
  ggtitle("Secondary diagnosis")
# number of diagnoses
ggplot(data %>% filter(number_diagnoses < 9),aes(x=number_diagnoses, fill=readmitted)) +
  geom_bar() +
  xlab("Number of diagnoses") +
  ggtitle("Number of diagnoses") +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73"))
# admission and discharge
ggplot(data, aes(x=admission_type_id, fill=readmitted)) +
  geom_bar() +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  xlab("Admission type ID") +
  ggtitle("Admission type ID")
ggplot(data %>% group_by(admission_source_id) %>% mutate(N = n()) %>% ungroup() %>% filter(N > 200),
       aes(x=admission_source_id, fill=readmitted)) +
  geom_bar() +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  xlab("Admission source ID") +
  ggtitle("Admission source ID")
ggplot(data %>% group_by(discharge_disposition_id) %>% mutate(N = n()) %>% ungroup() %>% filter(N > 250),
       aes(x=discharge_disposition_id, fill=readmitted)) +
  geom_bar() +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  xlab("Discharge disposition ID") +
  ggtitle("Discharge disposition ID")
# insulin change
ggplot(data, aes(x=insulin, fill=readmitted)) +
  geom_bar() +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  xlab("Insulin change") +
  ggtitle("Insulin change")
# change of medications
ggplot(data, aes(x=change, fill=readmitted)) +
  geom_bar() +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  xlab("Change of medications") +
  ggtitle("Change of medications")
# diabetic medications
ggplot(data, aes(x=diabetesMed, fill=readmitted)) +
  geom_bar() +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  xlab("Diabetic medications") +
  ggtitle("Diabetic medications")
# A1Cresults
ggplot(data, aes(x=A1Cresult, fill=readmitted)) +
  geom_bar() +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  xlab("A1C test result") +
  ggtitle("A1C test result")
# max glu serum
ggplot(data, aes(x=max_glu_serum, fill=readmitted)) +
  geom_bar() +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  xlab("max glu serum") +
  ggtitle("max glu serum")
# metformin
ggplot(data, aes(x=metformin, fill=readmitted)) +
  geom_bar() +
  scale_fill_manual(values=c("red", "#F0E442", "#009E73")) +
  xlab("Metformin") +
  ggtitle("Metformin")
# glimepiride
# glipizide
# glyburide
#########################################################################################
library(dplyr)
library(caret)
library(neuralnet)
library(nnet)
library(corrplot)
data = readRDS("Diabetic_cleaned.rds")
# correlation matrix
nummat = data %>% select_if(is.numeric)
cormat = cor(nummat)
corrplot(cormat)
# introduce dummy variables
#dummies <- dummyVars("~ .", data = data)
#dummies <- dummyVars("~ .", data = data, fullRank = T)
#dummy_data = predict(dummies, newdata = data)
# binary classification
data$readmitted = as.character(data$readmitted)
data = data %>% mutate(readmitted, readmitted = ifelse(readmitted == "NO", "NO", "YES"))
data$readmitted = as.factor(data$readmitted)
# create training and test set
trainIndex = createDataPartition(data$readmitted, p = .8, list = FALSE)
train_data = data[trainIndex,]
test_data = data[-trainIndex,]
# balancing
down_train = downSample(x = train_data[, -ncol(train_data)],
                        y = train_data$readmitted)
up_train = upSample(x = train_data[, -ncol(train_data)], y = train_data$readmitted)
# normalizing - uptrain
normalizing = preProcess(up_train, method=c("range"))
train_norm_up = predict(normalizing, up_train)
test_norm_up = predict(normalizing, test_data)
# normalizing - downtrain
normalizing = preProcess(down_train, method=c("range"))
train_norm_down = predict(normalizing, down_train)
test_norm_down = predict(normalizing, test_data)
# class distribution
round(table(data$readmitted)/nrow(data), 2) # <30 9%, >30 31%, NO 60%
# train and predict upsampled model
up_model = nnet(Class ~ ., data = train_norm_up, size = 10, MaxNWts = 9000, maxit =100)
up_pred = as.factor(predict(up_model, select(test_norm_up, -readmitted), type = "class"))
mean(up_pred == test_norm_up$readmitted)
confusionMatrix(up_pred, test_norm_up$readmitted)
# train and predcit downsampled model
down_model = nnet(Class ~ ., data = train_norm_down, size = 10, MaxNWts = 9000, maxit = 100)
down_pred = as.factor(predict(down_model, select(test_norm_down, -readmitted), type = "class"))
mean(down_pred == test_norm_down$readmitted)
confusionMatrix(down_pred, test_norm_down$readmitted)