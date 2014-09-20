---
title: "codeBook.MD"
author: "William Wilkins"
date: "Saturday, September 20, 2014"
output: html_document
---

Code Book for *run_analysis.R*
================================================================================
Course Project for Coursera's *Getting and Cleaning Data*
--------------------------------------------------------------------------------

##Description:
Describes the variables used in *run_analysis.R*.

#####Tables:
* features - contains the labels for the 561 features in the source data  
* activityLabels - contains the labels for the 6 experimental activities  
* subjectTrain - contains the IDs for the subjects used in the training set  
* X_train - contains the training set  
* y_train - contains the training labels 
* trainingSet - contains the training data, labels and subjects  
* subjectTest - contains the IDs for the subjects used in the test set  
* X_test - contains the test set  
* y_test - contains the test labels  
* testSet - contains the test data, labels and subjects  
* allData - contains the combined training and test sets   
* temp - a temporary copy of allData, minus the activity labels  
* avgData - the summarized (by mean) data from allData with activity and subject IDs and activity labels

####Vectors:  
* allColumns - a character vector containing the column names from allData 
* mean std cols (underscores omitted for presentation) - a logical vector identifying the ID columns and those columns whose data are the result of either a mean function or std function  
