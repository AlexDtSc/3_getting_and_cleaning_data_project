

##############################################################################
#
# FILE
#   run_analysis.R
#
# OVERVIEW
#   Using data collected from the accelerometers from the Samsung Galaxy S 
#   smartphone, work with the data and make a clean data set, outputting the
#   resulting tidy data to a file named "tidy_data.txt".
#   See README.md for details.
#

setwd("D:/Data Science/coursera_courses/3_Getting_and_cleaning_data/course_project/1")


library(dplyr)


##############################################################################
# STEP 0A - Get data
##############################################################################

# download zip file containing data if it hasn't already been downloaded


if (!file.exists(zipFile)) {
    download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
                  destfile = "UCI HAR Dataset.zip", mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist

if (!file.exists("UCI HAR Dataset")) {
    unzip(zipFile)
}

##############################################################################
# STEP 0B - Read data
##############################################################################

# read training data
trainingSubjects <- read.table(file.path("UCI HAR Dataset", "train", "subject_train.txt"))
head(trainingSubjects)
View(trainingSubjects)

trainingValues <- read.table(file.path("UCI HAR Dataset", "train", "X_train.txt"))
head(trainingValues)
View(trainingValues)

trainingActivity <- read.table(file.path("UCI HAR Dataset", "train", "y_train.txt"))
head(trainingActivity)
View(trainingActivity)

# read test data
testSubjects <- read.table(file.path("UCI HAR Dataset", "test", "subject_test.txt"))
head(testSubjects)
View(testSubjects)

testValues <- read.table(file.path("UCI HAR Dataset", "test", "X_test.txt"))
head(testValues)
View(testValues)

testActivity <- read.table(file.path("UCI HAR Dataset", "test", "y_test.txt"))
head(testActivity)
View(testActivity)

# read features, don't convert text labels to factors
features <- read.table(file.path("UCI HAR Dataset", "features.txt"), as.is = TRUE) ; class(features[ ,2]) # [1] "character", but not factor !!!
head(features)
View(features)

## note: feature names (in features[, 2]) are not unique
##       e.g. fBodyAcc-bandsEnergy()-1,8

# read activity labels
activities <- read.table(file.path("UCI HAR Dataset", "activity_labels.txt"))
head(activities)
View(activities)


colnames(activities) <- c("activityId", "activityLabel") # or names(activities) <- c("activityId", "activityLabel")
head(colnames(activities))

##############################################################################
# Step 1 - Merge the training and the test sets to create one data set
##############################################################################

# concatenate individual data tables to make single data table
humanActivity <- rbind(
    cbind(trainingSubjects, trainingValues, trainingActivity),
    cbind(testSubjects, testValues, testActivity)
)
head(humanActivity)
View(humanActivity)


# remove individual data tables to save memory
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

# assign column names

colnames(humanActivity) <- c("subject", features[, 2], "activity")
head(colnames(humanActivity))


##############################################################################
# Step 2 - Extract only the measurements on the mean and standard deviation
#          for each measurement
##############################################################################

# determine columns of data set to keep based on column name...
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

# ... and keep data in these columns only
humanActivity <- humanActivity[, columnsToKeep]
head(humanActivity)
View(humanActivity)


##############################################################################
# Step 3 - Use descriptive activity names to name the activities in the data
#          set
##############################################################################

# replace activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])


##############################################################################
# Step 4 - Appropriately label the data set with descriptive variable names
##############################################################################

# get column names
humanActivityCols <- colnames(humanActivity)
humanActivityCols
head(humanActivityCols)
View(humanActivityCols)

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)
humanActivityCols

# expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
humanActivityCols

# correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)
humanActivityCols

# use new labels as column names
colnames(humanActivity) <- humanActivityCols

View(humanActivity)


##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################

# group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
    group_by(subject, activity) %>%
    summarise_all(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
