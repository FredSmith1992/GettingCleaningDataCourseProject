##
## Getting and Cleaning Data - Course Project Requirements
##
## You should create one R script called run_analysis.R that does the following. 
##
## 1) Merges the training and the test sets to create one data set.
## 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3) Uses descriptive activity names to name the activities in the data set
## 4) Appropriately labels the data set with descriptive variable names. 
## 5) From the data set, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##
#############################################################################################################################################################
## NOTE: The Course Project Requirements tell you what the program *MUST* accomplish. They do *NOT* require you meet the requirements in a particular order.
#############################################################################################################################################################
##
## DISCLOSURES
##
## I received assistance from the following permitted sources:
##
## A)  From David's Project FAQ (https://class.coursera.org/getdata-010/forum/thread?thread_id=49) I was reminded to provide:
##    1) a run_analysis R script
##    2) a ReadMe markdown document
##    3) a Codebook markdown document
## from github.com/jtleek/datasharing
## Information about the variables (including units!) in the data set not contained in the tidy data
## Information about the summary choices you made
## Information about the experimental study design you used
## There should be a section called "Study design" that has a thorough description of how you collected the data.
## There is a section called "Code book" that describes each variable and its units.
##    4) a tidy data text file (updated to Coursera)
##    5) (in the ReadMe) "The Run_analysis script goes in the UCI directory which also contains the train and test folders"
##
## B) The Course Discussion Forum for the the Project (found on https://class.coursera.org/getdata-010/forum/thread?thread_id=381):
##    1) names(combined) <- c(as.character(feature[,2]), "subject", "label"
##


## Assumptions: you will have already...
## 1) downloaded the file https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
## 2) extracted the contents of the zip file
## 3) set your R session's working directory to that folder


library(data.table)
library(dplyr)

## REQUIREMENT #1 - Merge the training and the test sets to create one data set.
##

## First, get the descriptive information we'll need

## load column features text file, which describes the observation file contents
colNamesSource <- read.table("UCI HAR Dataset/features.txt",as.is=TRUE, header=FALSE)

## load activity labels test file, which provides the name for each activity
actLabelsSource <- read.table("UCI HAR Dataset/activity_labels.txt",header=FALSE)
colnames(actLabelsSource)=c("Activity","Activity Name")

## load the activity identifier column
trnActivities <- read.table("UCI HAR Dataset/train/y_train.txt",header=FALSE)
tstActivities <- read.table("UCI HAR Dataset/test/y_test.txt",header=FALSE)

## load the subject column
trnSubs <- read.table("UCI HAR Dataset/train/subject_train.txt",header=FALSE)
tstSubs <- read.table("UCI HAR Dataset/test/subject_test.txt",header=FALSE)

## load the training and test observations
trnObs <- read.table("UCI HAR Dataset/train/X_train.txt",header=FALSE)
tstObs <- read.table("UCI HAR Dataset/test/X_test.txt",header=FALSE)

## REQUIREMENT #4 Appropriately labels the data set with descriptive variable names. 
colnames(trnObs)=colNamesSource[,2]
colnames(tstObs)=colNamesSource[,2]

## REQUIREMENT #2 Extract only the measurements on the mean and standard deviation for each measurement. 
##
tstObsSubset<-tstObs[,c(grep(".*std()*",c(colNamesSource$V2)),grep(".*mean()*",c(colNamesSource$V2)))]
trnObsSubset<-trnObs[,c(grep(".*std()*",c(colNamesSource$V2)),grep(".*mean()*",c(colNamesSource$V2)))]

## create training observations data table
trnObsDT <- data.table(SetName = 'Training', Subject = trnSubs[,1], Activity = trnActivities[,1], trnObsSubset)

## create test observations data table
tstObsDT <- data.table(SetName = 'Test',     Subject = tstSubs[,1], Activity = tstActivities[,1], tstObsSubset)

## merge the two sets of observations. Using rbind (rather than cbind) I am opting for a long (vs wide) table
allObsDT <- rbind(tstObsDT,trnObsDT)

## REQUIREMENT #3 Uses descriptive activity names to name the activities in the data set
allObsDT <- inner_join(actLabelsSource,allObsDT)


## REQUIREMENT #5 From the data set, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
summaryDT <- tapply(summaryDT, *, mean)

write.table(allObsDT,file="SummaryDT.txt",row.name=FALSE)
