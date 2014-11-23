

##Setup Environment
rm(list=ls()) ## Clear Memory
setwd("~/DataProject_1") ##Set Working Directory
library(plyr) ##Load Package(s) Required for the Script


##Download Datafrom the Web
  url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" ##Define URL for data source
  download.file(url,"datasource.zip") ##Download Data
  unzip("datasource.zip", overwrite=TRUE) ##Unzip Data

##Define Path for Data Source
  ##Define Path for Data Source - Reference Data
    features_path<-"~/DataProject_1/UCI HAR Dataset/features.txt"
    activity_label_path<-"~/DataProject_1/UCI HAR Dataset/activity_labels.txt"

  ##Define Path for Data Source - Test
    subject_test_path<-"~/DataProject_1/UCI HAR Dataset/test/subject_test.txt"
    xtest_path<-"~/DataProject_1/UCI HAR Dataset/test/x_test.txt"
    ytest_path<-"~/DataProject_1/UCI HAR Dataset/test/y_test.txt"

  ##Define Path for Data Source - Train
    subject_train_path<-"~/DataProject_1/UCI HAR Dataset/train/subject_train.txt"
    xtrain_path<-"~/DataProject_1/UCI HAR Dataset/train/x_train.txt"
    ytrain_path<-"~/DataProject_1/UCI HAR Dataset/train/y_train.txt"

##Load Data from txt Files into Data Frames  
  ##Load Reference Data
    activity_label<- read.table(activity_label_path, header = FALSE) ##Load Activity Labels
    features<-read.table(features_path, header = FALSE) ##Load Features

  ##Load Data - Test
    subject_test<-read.table(subject_test_path, header = FALSE) ##Load Subject Test Data
    xtest_data_all <- read.table(xtest_path, header = FALSE)  ##Load X Test Data (all fields)
    ytest_data <- read.table(ytest_path, header = FALSE)  ##Load y Test Data   

  ##Load Data - Train  
    subject_train<-read.table(subject_train_path, header = FALSE) ##Load Subject Train Data
    xtrain_data_all <- read.table(xtrain_path, header = FALSE)  ##Load X Train Data (all fields)
    ytrain_data <- read.table(ytrain_path, header = FALSE) ##Load y Train Data   

##Name Columns in Datset    
  ##Name Reference Data Columns 
    names (features) <-  c("V1"="ID", "V2"="Label") ##Name Features Data Frame Columns
    features$Label=make.names(features$Label,unique=TRUE) ##Additional Step to Assure Features Label are Unique 
    names (activity_label) <-  c("V1"="ActivityLabelID", "V2"="ActivityLabel") ##Name Features Data Frame Columns

  ##Name Columns - Test
    names(subject_test)<-"SubjectID" ##Name Subject Test Data Frame Columns
    names(xtest_data_all)<-features$Label ##Name x test Data Frame Columns Using Features Data Frame
    names(ytest_data)<-"ActivityID" ##Name y Test Data Frame Column

  ##Name Columns - Train
    names(subject_train)<-"SubjectID" ##Name Subject Train Data Frame Columns
    names(xtrain_data_all)<-features$Label ##Name x Train Data Frame Columns Using Features Data Frame
    names(ytrain_data)<-"ActivityID"  ##Name y Train Data Frame Column
  
##Merge the training and the test sets to create one data set
  ##Merge X Data 
    xtest_data<-cbind(subject_test,xtest_data_all) ##Column Merge Subject to X Test Data
    xtrain_data<-cbind(subject_train,xtrain_data_all) ##Column Merge Subject to Train Data
    x_data<-rbind(xtest_data,xtrain_data) ##Row Merge X Test and Train

  ##Merge Y Data 
    y_data<-rbind(ytest_data,ytrain_data) ##Row Merge Y Data
    data<-cbind(y_data,x_data) ##Column Merge Activity Label to X Data

##Extracts only the measurements on the mean and standard deviation
  features_filter<-c(grep(c("std|mean"), features$Label)) ##Create Feature Label for Mean and Standard Deviation Columns
  data_Result<-data[, names(data) %in% c("ActivityID" ,"SubjectID",features[features_filter,]$Label)] ## Remove all Features except for mean and standard deviation Features 

##Apply Activity Label to Results
  tidy_data_Result<-merge(activity_label,data_Result,by.y="ActivityID",by.x="ActivityLabelID") ##Column Merge Activity Label to Test Activity Table


##Generate data set with the average of each variable for each activity and each subject
  aggregated_tidy_data_Result<-aggregate(tidy_data_Result, list(tidy_data_Result$SubjectID,tidy_data_Result$ActivityLabelID,tidy_data_Result$ActivityLabel), FUN=mean) ##Aggregate Results for Features by Subject ID and Activity
  aggregated_tidy_data_Result<-aggregated_tidy_data_Result[, !(colnames(aggregated_tidy_data_Result) %in% c("ActivityLabelID","ActivityLabel","SubjectID"))] ##Clean unwanted columns
  colnames(aggregated_tidy_data_Result)[1:3] <-c("SubjectID", "ActivityLabelID","ActivityName") ##Rename Columns for Clarity
  index<- with(aggregated_tidy_data_Result, order(SubjectID,ActivityLabelID)) ## Create index for sorting data by Subject and Activity
  aggregated_tidy_data_Result<-aggregated_tidy_data_Result[index, ] ##Sort Data

##Write Table to a Flat File
  write.table(aggregated_tidy_data_Result, file = "aggregated_tidy_data_Result.txt", sep = ",", row.name=FALSE)
