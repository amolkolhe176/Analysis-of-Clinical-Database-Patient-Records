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



# Fitting Logistic Regression to the Training set
set.seed(123)
c_glm = glm(readmitted ~ .,
            family = binomial,
            data = training_set)

summary(c_glm)

# Predicting the Test set results
prob_pred = predict(c_glm, type = 'response', newdata = test_set[-26])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
confusionMatrix(prob_pred,test_set$readmitted)
