library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)

# Step 1: Download, unzip, and process the raw data. 

fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileurl, destfile = "project.zip")

unzip("project.zip")

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

x_test <- read.table("UCI HAR Dataset/test/X_test.txt")

y_test <- read.table("UCI HAR Dataset/test/y_test.txt")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

x_train <- read.table("UCI HAR Dataset/train/X_train.txt")

y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

featuresvarname <- read.table("UCI HAR Dataset/features.txt")

# Step 2: Look at the dimension to fit and merge the data together.

dim(x_test)

dim(x_train)

dim(y_test)

dim(y_train)

dim(subject_test)

dim(subject_train)

# Step 3: Merge the training and the test sets to create one data set.

# Add column names to both x_train and y_train

colnames(x_train) <- featuresvarname$V2

colnames(x_test) <- featuresvarname$V2

subject <- rbind(subject_test, subject_train)

colnames(subject) <- "subject"

activity <- rbind(y_train, y_test)

colnames(activity) <- "activity"

features <- rbind(x_train, x_test)

data <- cbind(activity, subject, features)

data

View(data)

# Step 4: Extract only the measurements on the mean and standard deviation for each measurement

# identify the row numbers for mean and standard deviation within the features.txt file

# create a vector of column names

columnname <- colnames(data)

# # subset the measurements on the mean and std using grepl command

columnstokeep <- grep("(activity)|(subject)|(mean)|(std)", columnname)

data1 <- data[,columnstokeep]

data1

# Step 5: Use descriptive activity names to name the activities in the data set

# name the activity labels variables

colnames(activity_labels) <- c("activity", "activities")

# merge the two data table

data2 <- inner_join(activity_labels, data1, by = "activity")

# Step 6: Appropriately label the data set with descriptive variable names.

# Create a new column names vector

columnname2 <- colnames(data2)

# Edit the variable names by removing "-" and "()".

descriptivevarname <- columnname2 %>% 
  str_replace("\\(\\)", "") %>% 
  str_replace("-std", "-StdDev") %>% 
  str_replace("-mean", "-Mean") %>% 
  str_replace("activity", "ActivityID") 

colnames(data2) <- descriptivevarname

glimpse(data2)

# Step 7: From the data set in step 6, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

data3 <- data2 %>% 
  group_by(subject, activities) %>% 
  summarise_all(funs(mean)) %>% 
  arrange(subject, ActivityID) 

glimpse(data3)

# create an independent tidy dataset

write.table(data3, file = "tidydata.txt", row.name=FALSE)

