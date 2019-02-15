# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(data$readmitted, SplitRatio = 0.8)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

table(data$readmitted)


#Down Sample
set.seed(9560)
down_train <- downSample(x = training_set[, -ncol(training_set)],
                         y = training_set$readmitted)
table(down_train$Class) 

#up sample
set.seed(9560)
up_train <- upSample(x = training_set[, -ncol(training_set)],
                     y = training_set$readmitted)                         
table(up_train$Class) 


# Fitting Logistic Regression to the Training set
c_glm = glm(readmitted ~ .,
            family = binomial,
            data = training_set)

summary(c_glm)

# Predicting the Test set results
prob_pred = predict(c_glm, type = 'response', newdata = test_set[-10])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
confusionMatrix(prob_pred,test_set$readmitted)


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
                     #function used to measure performance
                     summaryFunction = twoClassSummary, #multiClassSummary for non binary
                     allowParallel = TRUE) #default looks for parallel backend

# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
library(rpart.plot)

#review root prob/entropy
table(data$readmitted)

m.rpart <- train(Class ~ ., 
                 trControl = ctrl,
                 metric = "ROC", #using AUC to find best performing parameters
                 preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
                 data = down_train, 
                 method = "rpart")
m.rpart

p.rpart <- predict(m.rpart,test_set)
confusionMatrix(p.rpart,test_set$readmitted)