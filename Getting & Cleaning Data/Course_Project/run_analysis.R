run_analysis <- function() {
  dir_path <- "./UCI HAR Dataset/"
  
  featureNames <- read.table(paste(dir_path, "features.txt", sep=""))
  activityLabels <- read.table(paste(dir_path, "activity_labels.txt", sep=""))
  testSubjectIDs <- read.table(paste(dir_path, "test/", "subject_test.txt", sep=""))
  trainSubjectIDs <- read.table(paste(dir_path, "train/", "subject_train.txt", sep=""))
  testActivityIDs <- read.table(paste(dir_path, "test/", "y_test.txt", sep=""))
  trainActivityIDs <- read.table(paste(dir_path, "train/", "y_train.txt", sep=""))
  testFeaturesData <- read.table(paste(dir_path, "test/", "X_test.txt", sep=""))
  trainFeaturesData <- read.table(paste(dir_path, "train/", "X_train.txt", sep=""))
  
  combineFeaturesData <- rbind(testFeaturesData, trainFeaturesData)
  combineSubjectIDs <- rbind(testSubjectIDs, trainSubjectIDs)
  combineActivityIDs <- rbind(testActivityIDs, trainActivityIDs)
  
  names(activityLabels) <- c("ActivityID", "ActivityName")
  names(combineFeaturesData) <- featureNames$V2
  names(combineSubjectIDs) <- c("Subject")
  names(combineActivityIDs) <- c("ActivityID")

  combinedData <- cbind(combineFeaturesData, combineSubjectIDs, combineActivityIDs)
  combinedData <- merge(combinedData,activityLabels,by='ActivityID',all.x=TRUE)
  colNames <- names(combinedData)
  colVector <- grepl("mean", colNames) | grepl("std", colNames) | grepl("Subject", colNames) | grepl("ActivityName", colNames)
  data <- combinedData[, colVector == TRUE]
  
  names(data)<-gsub("^t", "Time", names(data))
  names(data)<-gsub("^f", "Frequency", names(data))
  names(data)<-gsub("Acc", "Accelerometer", names(data))
  names(data)<-gsub("Gyro", "Gyroscope", names(data))
  names(data)<-gsub("Mag", "Magnitude", names(data))
  names(data)<-gsub("BodyBody", "Body", names(data))
  return(data)
}

tidy_data <- function(data) {
  refinedData<-aggregate(. ~Subject + ActivityName, data, mean)
  tidyData<-refinedData[order(refinedData$Subject,refinedData$ActivityName),]
  write.table(tidyData, file = "tidy_data.txt", row.name=FALSE)
  return(tidyData)
}