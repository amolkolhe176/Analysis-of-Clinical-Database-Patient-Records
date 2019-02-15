# load packages
library(data.table)
library(lubridate)
library(dplyr)

setwd("C:/Users/Felix/Desktop/Data Science & Big Data Analytics/Group Project [diabetes]")

###########################################################################################
#                                                                                         #
#                                     DATA PREPARATION                                    # 
#                                                                                         #
###########################################################################################

# read in data
data = fread(file = "diabetic_data.csv", 
                  #data.table = FALSE,
                  #nrows = 100,
                  drop = c("encounter_id", "weight", "payer_code", "examide", "citoglipton", "medical_specialty"),
                  stringsAsFactors = TRUE)

# convert int to right type factor
data$discharge_disposition_id = as.factor(data$discharge_disposition_id)
data$admission_type_id        = as.factor(data$admission_type_id)
data$admission_source_id      = as.factor(data$admission_source_id)

# remove observations with unknown gender
data = data %>% filter(gender != "Unknown/Invalid") # 3 removed

### RACE

  ## table(data$race) 
  # 2273 unknown -> drop observations?
#added others in the place of "?"
data$race <- as.character(data$race)
data$race[data$race == "?"] <- "Others"


  # remove observations with unknown race
  ## data = data %>% filter(race != "?")

### MEDICAL SPECIALTY  COlumn DROPED

# Adding "NONE" Diagnosis columns
data$diag_1 <- as.character(data$diag_1)
data$diag_1[data$diag_1 == "?"] <- "NONE"
data$diag_1 <- as.factor(data$diag_1)

data$diag_2 <- as.character(data$diag_2)
data$diag_2[data$diag_2 == "?"] <- "NONE"
data$diag_2 <- as.factor(data$diag_2)

data$diag_3 <- as.character(data$diag_3)
data$diag_3[data$diag_3 == "?"] <- "NONE"
data$diag_3 <- as.factor(data$diag_3)

### PATIENT NUMBER
# 
length(unique(data$patient_nbr))
# patient_nbr is not unique, just 70k patients but 100k observations

#  
# (We should not eliminate the multiple entries)we shoud eliminate multiple entries for one patient so that all observations are independent
# they write something in the paper about it, pls read
# Patient_nbr is unique but the observations can be more than the patient_nbr as the patient might come back 
# and same unique number is used
#
     
### MEDICAL STUFF
#
# these are very unbalanced, some even just have one "other" value, shall we drop them?
# then add them at the top into the fread call
# 
# I didnt list 2 or 3 features where another value had at least 4.000
# similar to pioglitazonebelow

# glyburide-metformin
#  Down     No        Steady     Up 
#  6        101060    692        8 

# tolazamide
# No         Steady     Up 
# 101724     38         1

# acarbose
# Down     No        Steady     Up 
# 3        101455    295        10

# troglitazone
# No          Steady 
# 101760      3 

# miglitol
# Down     No         Steady     Up 
# 5        101725     31         2

# pioglitazone
# Down     No      Steady     Up 
# 118      94438   6976       234

# rosiglitazone
# Down     No      Steady     Up 
# 87       95401   6100       178 

# tolbutamide
# No         Steady 
# 101743     23 

# nateglinide
# Down     No        Steady     Up 
# 11       101063    668        24

# repaglinide
# Down     No       Steady     Up 
# 45       100227   1384       110

# chlorpropamide
# Down     No       Steady     Up 
# 1        101680   79         6

# acetohexamide      
# No          Steady 
# 101765      1        
   
# metformin-pioglitazone
# No          Steady 
# 101765      1 

# metformin-rosiglitazone
# No          Steady 
# 101764      2
    
# glipizide-metformin
# No          Steady 
# 101753      13
    
# glimepiride-pioglitazone
# No          Steady 
# 101765      1      

library(rpart)
library("rpart.plot")

library(caTools)
set.seed(123)
data_Split <- data[1:100,34:44]
fit <- rpart( readmitted ~ ., 
             method="class", 
             data=data_Split,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))
fit
rpart.plot(fit)
