---
title: "README.MD"
author: "William Wilkins"
date: "Saturday, September 20, 2014"
output: html_document
---

README for *run_analysis.R*
================================================================================
Course Project for Coursera's *Getting and Cleaning Data*
--------------------------------------------------------------------------------

##Description

Summary information about the steps used to obtain, clean and transform the
data provided into a tidy data set for downstream analysis.

##Data Source
**Data were provided at:**
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

**Source Data:**
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Using a group of thirty volunteers, aged 19 to 48 years, the data are the 
record of an experiment in which each of the volunteers performed the
following tasks:  

1. WALKING  
2. WALKING_UPSTAIRS  
3. WALKING_DOWNSTAIRS  
4. SITTING  
5. STANDING  
6. LAYING  

During these activities, each volunteer wore a Samsung Galaxy S II on the waist.

**Data Captured:**  
1. Triaxial acceleration from the accelrometer (total acceleration) and the estimated body acceleration.  
2. Triaxial Angular velocity from the gyroscope.  
3. A 561-feature vector with time and frequency domain variables.  
4. Its activity label.  
5. An identifier of the subject who carried out the experiment.

Using the data obtained and described above, *run_analysis.R* achieves the 
following objectives (labeled in the script with "*PROJECT OBJECTIVE*").

1. Merges the training and the test sets to create one data set.  
2. Extracts only the measurements on the mean and standard deviation for each measurement.  
3. Uses descriptive activity names to name the activities in the data set.  
4. Appropriately labels the data set with descriptive variable names.  
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.  

####PROJECT OBJECTIVE 1 - Merges the training and the test sets to create one data set.
Having obtained the source data as noted above, I read the following files from
that data into tables:
* features.txt - to be used as column names  
* activity_labels.txt - labels for the six experimental activities  
* subject_train.txt - identifies volunteers used to provide the training data set  
* x_train.txt - the training data set  
* y_train.txt - the training set labels
* subject_test.txt - identifies volunteers used to provide the test data set  
* x_test.txt - the test data set  
* y_text.txt - the test set labels  

Once I had the training-related data in tables, I added appropriate column
names to each of the tables. I repeated this process for the test-related
tables.

To link the experiment data with their activities and subjects, I created two
new tables, one for the training set and one for the test set, that combined the 
training/test data with the training/test activities and subjects.

With both the training and test data now reshaped and similar, I combined the
two tables into a single data set.

####PROJECT OBJECTIVE 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
To meet this objective, I interpreted the variable names in the combined data 
set, most of which contained either "mean" or "std" as part of the variable 
name, such that only those that appeared to have applied a mean or std
function to the data in the variable would be included in the extract.

I created a logical vector that contained **TRUE** for the IDs (activity and
subject) and for variables with "mean()" and "std()" in the variable name.
Using the logical vector, I subsetted the combined data to include only those
columns that met the conditions described.

####PROJECT OBJECTIVE 3 - Uses descriptive activity names to name the activities in the data set.
I merged the newly obtained data with the activity labels as found in the 
activities_labels.txt file to provide descriptive activity names to the data.

####PROJECT OBJECTIVE 4 - Appropriately labels the data set with descriptive variable names.
I wanted the mean and std variables to contain "Mean" or "StdDev" in the name,
and used gsub to replace the "-mean()" and "-std()" patterns that were
originally present in the variable names. I also noted a number of the variables
repeated the word "Body" multiple times, and I reduced these to a single
occurance of the word "Body", also using gsub. Additionally, as some variables
appeared to be classified as to time or frequency, denoted by "t" or "f" at the
start of the variable name, I used gsub to expand the labels to include either 
"time" or "frequency" as appropriate.

####PROJECT OBJECTIVE 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
I interpreted this instruction to mean that the file should contain identifiers
for activity and subject independently of the original data set, as well as a
computed average for each of the mean/std variables I had earlier extracted.

I temporarily removed the activity labels, as they were in the way of producing
the calculations in the way I had chosen to do it (aggregate function). Using 
aggregate function, I computed the means for each of the "mean" and "std"
variables present in the extract for each activity and for each subject.

I added back the activity labels to be in compliance with Objective 3 above,
and then exported the final tidy data set to a tab-delimited file called
*avgData*.