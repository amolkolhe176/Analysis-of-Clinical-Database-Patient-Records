###########################################################################################
#                                                                                         #
#                                     DATA PREPARATION                                    #
#                                                                                         #
###########################################################################################

library(dplyr)

# read in data
data <- read.csv("diabetic_data.csv")

# drop columns
data = data %>% select(-c("encounter_id", "weight", "payer_code", "citoglipton", "medical_specialty","glyburide.metformin",
            "tolazamide", "acarbose", "troglitazone", "miglitol", "pioglitazone", "rosiglitazone", "tolbutamide", "nateglinide",
            "repaglinide","chlorpropamide","acetohexamide","metformin.pioglitazone","metformin.rosiglitazone","glipizide.metformin",
            "glimepiride.pioglitazone", "examide", "diag_3"))

# convert int to correct type factor
data$discharge_disposition_id = as.factor(data$discharge_disposition_id)
data$admission_type_id        = as.factor(data$admission_type_id)
data$admission_source_id      = as.factor(data$admission_source_id)

# remove duplicate records for patient_nbr
data = data %>% distinct(patient_nbr, .keep_all = TRUE) %>% select(-patient_nbr)

# remove observations with unknown gender or race or diagnosis
data = data %>% filter(gender != "Unknown/Invalid") # 3 removed
data = data %>% filter(race != "?") # 2000 removed
data = data %>% filter(diag_1 != "?") # 110 removed
data = data %>% filter(diag_2 != "?") # 300 removed

# drop unused factor levels
data = droplevels(data)

# group primary diagnosis into international classification groups
data$diag_1 = as.character(data$diag_1)
data = data %>%  
  mutate(diag_1 = replace(diag_1, startsWith(diag_1, "V"), "Vcode")) %>% 
  mutate(diag_1 = replace(diag_1, startsWith(diag_1, "E"), "Ecode")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 139, "A")) %>%
  mutate(diag_1 = replace(diag_1, diag_1 <= 239, "B")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 279, "C")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 279, "D")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 289, "E")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 319, "F")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 389, "G")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 459, "H")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 519, "I")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 579, "J")) %>%
  mutate(diag_1 = replace(diag_1, diag_1 <= 629, "K")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 679, "L")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 709, "M")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 739, "N")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 759, "O")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 779, "P")) %>% 
  mutate(diag_1 = replace(diag_1, diag_1 <= 799, "Q")) %>%
  mutate(diag_1 = replace(diag_1, diag_1 <= 999, "R"))
data$diag_1 = as.factor(data$diag_1)

# group primary diagnosis into international classification groups
data$diag_2 = as.character(data$diag_2)
data = data %>%  
  mutate(diag_2 = replace(diag_2, startsWith(diag_2, "V"), "Vcode")) %>% 
  mutate(diag_2 = replace(diag_2, startsWith(diag_2, "E"), "Ecode")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 139, "A")) %>%
  mutate(diag_2 = replace(diag_2, diag_2 <= 239, "B")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 279, "C")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 279, "D")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 289, "E")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 319, "F")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 389, "G")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 459, "H")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 519, "I")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 579, "J")) %>%
  mutate(diag_2 = replace(diag_2, diag_2 <= 629, "K")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 679, "L")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 709, "M")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 739, "N")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 759, "O")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 779, "P")) %>% 
  mutate(diag_2 = replace(diag_2, diag_2 <= 799, "Q")) %>%
  mutate(diag_2 = replace(diag_2, diag_2 <= 999, "R")) 
data$diag_2 = as.factor(data$diag_2)

# Save an object to a file
saveRDS(data, file = "Diabetic_cleaned.rds")

#data = readRDS("Diabetic_cleaned.rds")