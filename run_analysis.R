# Getting and Cleaning Data Course Project
#
# The purpose of this project is to demonstrate your ability to collect, work with, 
# and clean a data set. The goal is to prepare tidy data that can be used for later 
# analysis. 
# You will be graded by your peers on a series of yes/no questions related to the project. 
# You will be required to submit: 
#   1) a tidy data set as described below, 
#   2) a link to a Github repository with your script for performing the analysis, and 
#   3) a code book that describes the variables, the data, and any transformations or work
#      that you performed to clean up the data called CodeBook.md. 
# 
# You should also include a README.md in the repo with your scripts. 
# This repo explains how all of the scripts work and how they are connected.
# 
# One of the most exciting areas in all of data science right now is wearable computing - 
# see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing
# to develop the most advanced algorithms to attract new users. The data linked to from the
# course website represent data collected from the accelerometers from the Samsung Galaxy S
# smartphone. A full description is available at the site where the data was obtained:
#   
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Here are the data for the project:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following.
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.

# Review criteria
#
# The submitted data set is tidy.
# The Github repo contains the required scripts.
# GitHub contains a code book that modifies and updates the available codebooks with the data
# to indicate all the variables and summaries calculated, along with units, and any other relevant information.
# The README that explains the analysis files is clear and understandable.
# The work submitted for this project is the work of the student who submitted it.


# 0a. get data

setwd("C:/Users/admin/Desktop/Getting & cleaning data/ProjectUCI")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./Temp.zip")
unzip(zipfile=".//Temp.zip",exdir="C:/Users/admin/Desktop/Getting & cleaning data/ProjectUCI")

# turn off warnings
options(warn=-1)

# 0b. load packages
library(dplyr)
library(tidyr)
library(data.table)

# 0c. read data
setwd("C:/Users/admin/Desktop/Getting & cleaning data/ProjectUCI/UCI HAR Dataset/train")
  dataSubjectTrain<-tbl_df(read.table("subject_train.txt"))
  dataX_Train<-tbl_df(read.table("X_train.txt"))
  dataY_Train<-tbl_df(read.table("y_train.txt"))
  
setwd("C:/Users/admin/Desktop/Getting & cleaning data/ProjectUCI/UCI HAR Dataset/test")    
  dataSubjectTest<-tbl_df(read.table("subject_test.txt"))
  dataX_Test<-tbl_df(read.table("X_test.txt"))
  dataY_Test<-tbl_df(read.table("y_test.txt"))
  
setwd("C:/Users/admin/Desktop/Getting & cleaning data/ProjectUCI/UCI HAR Dataset")
  features<- tbl_df(read.table("features.txt"))
  names(features)<-c("featNum", "featName")

  actLabel<- tbl_df(read.table("activity_labels.txt"))
  names(actLabel)<- c("actNum","actName")
  
setwd("C:/Users/admin/Desktop/Getting & cleaning data/ProjectUCI")

# ----------------------------------------------------------------
# 1. Merge the training and the test set to create one data set

Subjects <- rbind(dataSubjectTrain, dataSubjectTest)
names(Subjects)
names(Subjects)[names(Subjects)=="V1"] <- "subject"

# y gives the activity
#  Activities
#   1 WALKING
#   2 WALKING_UPSTAIRS
#   3 WALKING_DOWNSTAIRS
#   4 SITTING
#   5 STANDING
#   6 LAYING
Acts <- rbind(dataY_Train,dataY_Test)
names(Acts)[names(Acts)=="V1"] <- "actNum"

# X gives the data values
X <- rbind(dataX_Train,dataX_Test)
names(X) <- features$featName

temp<- cbind(Subjects, Acts)
data <- cbind(temp, X)


#  ----------------------------------------------------------------------------------------
# 2. Extract only the measurements on the mean and standard deviation for each measurement.

fMS <- grep("mean\\(\\)|std\\(\\)",features$featName,value=TRUE)

fMeanStd <- union(c("subject","actNum"), fMS)
data<- subset(data,select=fMeanStd) 
data[1:3, 1:8]

#  ----------------------------------------------------------------------------------------
# 3. Uses descriptive activity names to name the activities in the data set
#     add column with activity names to match the numbers in column actNum
data <- merge(actLabel,data,by="actNum", all.x=TRUE)
data[1:3, 1:8]

#  ----------------------------------------------------------------------------------------
# 4. Appropriately labels the data set with descriptive variable names.

names(data)<-gsub("std()", "STD", names(data))
names(data)<-gsub("mean()", "MEAN", names(data))
names(data)<-gsub("Acc", "Accelerometer", names(data))
names(data)<-gsub("Gyro", "Gyroscope", names(data))
names(data)<-gsub("Mag", "Magnitude", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))
# now do time and freq
names(data)<-gsub("^t", "time", names(data))
names(data)<-gsub("^f", "freq", names(data))
names(data)


#  ----------------------------------------------------------------------------------------
# 5. From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.

dataMean<- data[c(1,2,3,grep("MEAN\\(\\)",names(data)))]
dataMeanSort<- tbl_df(arrange(dataMean,subject,actName))
head(dataMeanSort,2)
write.table(dataMeanSort, "TidyMeanData.txt", row.name=FALSE)







