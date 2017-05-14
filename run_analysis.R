#create one R script called run_analysis.R that does the following.

#1 Merges the training and the test sets to create one data set.
#2 Extracts only the measurements on the mean and standard deviation for each measurement.
#3 Uses descriptive activity names to name the activities in the data set
#4 Appropriately labels the data set with descriptive variable names.
#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(data.table)


fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Download and unzip the data file to local directory
temp <- tempfile()
download.file(fileUrl, temp, method="curl")
unzip(temp)
rm(temp)

# Read the data sets train and test
filepath <- "./UCI HAR Dataset"

# Read activity data
testActDt  <- read.table(file.path(filepath, "test" , "Y_test.txt" ),header = FALSE)
trainActDt <- read.table(file.path(filepath, "train", "Y_train.txt"),header = FALSE)

testSubDt  <- read.table(file.path(filepath, "test" , "subject_test.txt" ),header = FALSE)
trainSubDt <- read.table(file.path(filepath, "train", "subject_train.txt"),header = FALSE)

testFeatureDt  <- read.table(file.path(filepath, "test" , "X_test.txt" ),header = FALSE)
trainFeatureDt <- read.table(file.path(filepath, "train", "X_train.txt"),header = FALSE)

# 1) 

# Merge by rows
actDt <- rbind(testActDt, trainActDt)
subDt <- rbind(testSubDt, trainSubDt)
featureDt <- rbind(testFeatureDt, trainFeatureDt)

# set columns names
names(actDt) <- c("Activity")
names(subDt) <- c("Subject")
featureNames <- read.table(file.path(filepath, "features.txt"),head=FALSE)
names(featureDt)<- featureNames$V2

# Merge all columns together to get one large data set
subact <- cbind(actDt, subDt)
allDt <- cbind(featureDt, subact)


# 2) 
extract_features <- featureNames$V2[grep("mean\\(\\)|std\\(\\)", featureNames$V2)]

extractedNames<-c(as.character(extract_features), "Subject", "Activity" )
extractedData<-subset(allDt,select=extractedNames)


# 3) # Uses descriptive activity names to name the activities in the data set
# read features

activityLabels <- read.table(file.path(filepath, "activity_labels.txt"),header = FALSE)
allDt$Activity <- factor(allDt$Activity, 
                    levels = c(1,2,3,4,5,6),
                    labels = activityLabels$V2)

# 4) Appropriately labels the data set with descriptive variable names.
names(allDt)<-gsub("^t", "time", names(allDt))
names(allDt)<-gsub("^f", "frequency", names(allDt))
names(allDt)<-gsub("Acc", "Accelerometer", names(allDt))
names(allDt)<-gsub("Gyro", "Gyroscope", names(allDt))
names(allDt)<-gsub("Mag", "Magnitude", names(allDt))
names(allDt)<-gsub("BodyBody", "Body", names(allDt))

# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(plyr)
tidyDt <-aggregate(. ~Subject + Activity, allDt, mean)
tidyDt <-tidyDt[order(allDt$Subject,allDt$Activity),]
write.table(tidyDt, file = "tidydata.txt",row.name=FALSE)