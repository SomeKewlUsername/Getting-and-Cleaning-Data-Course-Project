library(dplyr)

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip if data directory doesn't exist already.
dataFolder <- "UCI HAR Dataset"
if (!file.exists(dataFolder)) {
  unzip(zipFile)
}

# Reading data
X_trainSubs <- read.table(file.path(dataFolder, "train", "subject_train.txt"))
X_train <- read.table(file.path(dataFolder, "train", "X_train.txt"))
X_trainAct <- read.table(file.path(dataFolder, "train", "y_train.txt"))

X_testSubs <- read.table(file.path(dataFolder, "test", "subject_test.txt"))
X_test <- read.table(file.path(dataFolder, "test", "X_test.txt"))
X_testAct <- read.table(file.path(dataFolder, "test", "y_test.txt"))

features <- read.table(file.path(dataFolder, "features.txt"), as.is = TRUE)

activities <- read.table(file.path(dataFolder, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

## Merging training and test data to create one data set
humanActivity <- rbind(cbind(X_trainSubs, X_train, X_trainAct),
                       cbind(X_testSubs, X_test, X_testAct))

# remove individual data tables to save memory
rm(X_trainSubs, X_train, X_trainAct, 
   X_testSubs, X_test, X_testAct)

colnames(humanActivity) <- c("subject", features[, 2], "activity")

## Extracting mean and standard deviation measurements
keepCols <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, keepCols]

## Assigning Acitivy names
humanActivity$activity <- factor(humanActivity$activity, levels = activities[, 1], labels = activities[, 2])

## Assigning Variable names
humanActivityCols <- colnames(humanActivity)
humanActivityCols <- humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

## Abbreviations and cleaning up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

colnames(humanActivity) <- humanActivityCols

## Creating tidy set with averages
humanActivityMeans <- humanActivity %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

## Writing "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)