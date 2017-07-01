## This script creates neat, tidy data from data files related to wearable
## electronics.  Please place this script in the same folder as the 
## 'train' and 'test' folders.

library(stats)
library(base)
library(readr)
library(dplyr)

home <- getwd()

## Reads in variable names from features.txt
feats <- read_table2("features.txt", col_names = FALSE)
feats_names <- feats$X2
feats_names <- paste(3:563, feats_names)
## moves to train folder and reads in data. formats subject, activity, and data.
setwd("train")
subject_train <- read_table2("subject_train.txt", col_names = FALSE)
X_train <- read_table2("X_train.txt", col_names = FALSE)
y_train <- read_table2("y_train.txt", col_names = FALSE)
train_data <- cbind(subject_train, y_train, X_train)

## moves to test folder and reads in data. formats subject, activity, and data.
## binds together training and test data. formats the total_data data frame
## with new variable names to allow tibble creation.
setwd(home)
setwd("test")
subject_test <- read_table2("subject_test.txt", col_names = FALSE)
X_test <- read_table2("X_test.txt", col_names = FALSE)
y_test <- read_table2("y_test.txt", col_names = FALSE)
test_data <- cbind(subject_test, y_test, X_test)

total_data <- rbind(train_data, test_data) ## FINISHED OBJECTIVE 1
colnames(total_data) <- c("Subject", "Activity_Type", feats_names)
total_data <- as_tibble(total_data, validate = TRUE)

## moves back to home directory and selects subject, activity type, and 
## variables with names containing 'mean' and 'std'.
setwd(home)
mean_std <- select(total_data, Subject, Activity_Type, 
                                 contains("mean()"), contains("std()"), 
                                -contains("Freq"))  ## FINISHED OBJ 2

##Replaces Activity Types by number code with character descriptions.
mean_std_actnames <- mean_std %>% 
        mutate(Activity_Type = replace(Activity_Type, Activity_Type == 1,
                                       "1 WALKING")) %>%
        mutate(Activity_Type = replace(Activity_Type, Activity_Type == 2,
                                       "2 WALKING_UPSTAIRS")) %>%
        mutate(Activity_Type = replace(Activity_Type, Activity_Type == 3,
                                       "3 WALKING_DOWNSTAIRS")) %>%
        mutate(Activity_Type = replace(Activity_Type, Activity_Type == 4,
                                       "4 SITTING")) %>%
        mutate(Activity_Type = replace(Activity_Type, Activity_Type == 5,
                                       "5 STANDING")) %>%
        mutate(Activity_Type = replace(Activity_Type, Activity_Type == 6,
                                       "6 LAYING")) ## FINISHED OBJ 3

## Removes numbers from measurement variable names to make them clearer.
name <- colnames(mean_std_actnames)
new_name <- gsub('[[:digit:]]+ ', '', name)
colnames(mean_std_actnames) <- new_name  ## FINISHED OBJ 4

## Groups the data by Subject and Activity Type, then calculates the mean of 
## each variable.  Writes the output to a txt file for submission/review.
result <- mean_std_actnames %>% 
        group_by(Subject, Activity_Type) %>%
        summarise_all(mean)
write.table(result, file = "Accelerometer_Tidy_Data.txt", row.names = FALSE)