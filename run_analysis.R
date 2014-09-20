# Set your working directory into the (unzipped) UCI HAR Dataset folder before 
# running this script.

# Clean up the workspace -- comment out this command if you don't want to
# execute this action. It will remove data currently present in your
# workspace environment.
rm(list=ls())

# Get features and activity labels.
features <- read.table("./features.txt", header = FALSE)
activityLabels <- read.table("./activity_labels.txt", header = FALSE)

# Get training data.
subjectTrain <- read.table("./train/subject_train.txt", header = FALSE)
X_train <- read.table("./train/x_train.txt", header = FALSE)
y_train <- read.table("./train/y_train.txt", header = FALSE)

# Set column names for use on the training data. Make sure activityLabels and 
#y_train share a common ID column.
colnames(activityLabels) <- c("activityID", "activityType")
colnames(subjectTrain) <- "subjectID"
colnames(X_train) <- features[, 2]
colnames(y_train) <- "activityID"

# Combine y_train, subjectTrain and X_train to get a reshaped training set.
trainingSet <- cbind(y_train, subjectTrain, X_train)

# Get test data.
subjectTest <- read.table("./test/subject_test.txt", header = FALSE)
X_test <- read.table("./test/x_test.txt", header = FALSE)
y_test <- read.table("./test/y_test.txt", header = FALSE)

# Set column names for use on the test data. Make sure activityLabels and
#y_test share a common ID column.
colnames(subjectTest) <- "subjectID"
colnames(X_test) <- features[, 2]
colnames(y_test) <- "activityID"

# Combine y_test, subjectTest, and X_test to get a reshaped test set.
testSet <- cbind(y_test, subjectTest, X_test)

# PROJECT OBJECTIVE 1 - Merges the training and the test sets to create one 
# data set.
allData <- rbind(trainingSet, testSet)

# Get the column names in order to find the mean and std columns.
allColumns <- colnames(allData)

# Find the columns that measure mean or std. Include activity and subject IDs,
# but exclude columns use "mean" or "std" in the column name, but do not
# appear to measure a mean or std.
mean_std_cols <- (grepl("activity..", allColumns) | 
                           grepl("subject..", allColumns) | 
                           grepl("-mean..", allColumns) & 
                           !grepl("-meanFreq..", allColumns) & 
                           !grepl("mean..-", allColumns) | 
                           grepl("-std", allColumns) & 
                           !grepl("-std()..-", allColumns));
                  
# PROJECT OBJECTIVE 2 - Extracts only the measurements on the mean and standard 
# deviation for each measurement.

# Subset the data to use only those columns that are a measure of mean or std.
allData <- allData[mean_std_cols == TRUE];

# PROJECT OBJECTIVE 3 - Uses descriptive activity names to name the activities 
# in the data set.

# Merge allData with activityLabels to provide descriptive activity names
# for the activities in the data set.
allData <- merge(allData, activityLabels, by = "activityID", all.x = TRUE)

# Update column names to match up after merge.
allColumns <- colnames(allData)

# PROJECT OBJECT 4 - Appropriately labels the data set with descriptive variable 
# names.

# Replacements: t = time, f = frequency, Mag = Magnitute, mean() = Mean,
# std() = StdDev, fixed repeates of "Body", removed parentheses.
for (i in 1 : length(allColumns)) {
        allColumns[i] <- gsub("\\()", "", allColumns[i])
        allColumns[i] <- gsub("^(t)", "time", allColumns[i])
        allColumns[i] <- gsub("^(f)", "freqency", allColumns[i])
        allColumns[i] <- gsub("Mag", "Magnitude", allColumns[i])
        allColumns[i] <- gsub("-mean", "Mean", allColumns[i])
        allColumns[i] <- gsub("-std", "StdDev", allColumns[i])
        allColumns[i] <- gsub("BodyBody", "Body", allColumns[i])
}

# Replace allData's column names with the new ones.
colnames(allData) <- allColumns;

# Temporarily remove activity type in preparation for calculate averages
temp <- allData[, names(allData) != "activityType"];

#PROJECT OBJECTIVE 5 - From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable for each activity 
# and each subject.

# Summarize the remaining data to include the average of each activity variable
# for each subject
avgData <- aggregate(temp[, names(temp) != c("activityID", "subjectID")], 
                  by = list(activityID = temp$activityID,
                            subjectID = temp$subjectID), mean);

# Return the activity type column
avgData <- merge(avgData, activityLabels, by = "activityID", all.x = TRUE);

# Write the tidy data set out to a file.
write.table(avgData, "./avgData.txt", row.names = FALSE, sep = "\t");
