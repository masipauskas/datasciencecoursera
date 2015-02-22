if (!exists("doNotWriteToFile")) doNotWriteToFile <- FALSE

# Loads the requested dataset (train / test) based on dataSetName, and dataRoot.
# dataSetName - name of the dataSet to load (train or test)
# featureColumnLabels - labels to apply to each of the elements in feature Vecotor
# dataRoot - where is the root directory for data, empty for current working directory, for development data/ has been used
# returns a tidy dataSet.
loadDataSet <- function(dataSetName, featureColumnLabels, dataRoot) {
  #utility functions region
  # takes in the data frame, and returns a vector of standard deviations for each of the rows
  util.rowSD <- function(dataFrame) {
    apply(dataFrame, 1, sd) 
  }
  
  # takes in the data frame, and returns a vector of means for each of the rows
  util.rowMean <- function(dataFrame) {
    apply(dataFrame, 1, mean) 
  }
  
  #gets the file name, for each attribute - based on attribute name, data set name and dataRoot
  util.getFileName <- function(attributeName, intermediateDir = "") {
    paste(dataRoot, dataSetName, "/", intermediateDir, attributeName, "_", dataSetName, ".txt", sep = "")
  }
  # end of util funtions
  
  # get the dataSet from a given attribute fileName, in the specified innerDir and then returns nicely labeled dataSet with Mean and Standard deviation
  # from the original dataset, where naming is {activityName}.Mean / {ActivityName}.SD
  util.getConsolidatedDataSet <- function(activityName, fileName, innerDir = "") {
    dataSet <- read.table(file = util.getFileName(fileName, innerDir))
    dataSetMean <- util.rowMean(dataSet)
    dataSetSD <- util.rowSD(dataSet)
    
    result <- cbind(dataSetMean, dataSetSD)
    colnames(result) <- c(paste(activityName, ".Mean", sep = ""), paste(activityName, ".SD", sep = ""))
    result
  }
  
  #end of util functions
  
  # get subject and activity data
  subjectTable <- read.table(file = util.getFileName("subject"), col.names = c("Subject"), stringsAsFactors = TRUE)
  activityTable <- read.table(file = util.getFileName("y"), col.names = c("Activity ID"), stringsAsFactors = TRUE)
  
  # get the feature vector
  featureVectorDataSet <- read.table(file = util.getFileName("x"))
  colnames(featureVectorDataSet) <- featureColumnLabels
  
  # merge all results into a single data frame
  result <- cbind(subjectTable, activityTable, 
                  featureVectorDataSet)
  result
}

# Runs analysis on whole data set:
# - loads and cleans the training and test data sets
# - merges them together
# - adds activity labels
# - writes the resulting clean data set as "analysis.txt"
# Args: 
#   dataRoot - location of dataSet to be analysed, default - current working directory
#   resultFileName - fileName of resulting data set to be writtend, default - analysis.txt
runAnalysis <- function(dataRoot = "UCI HAR Dataset/", resultFileName = "analysis.txt") {
  # get a vector of each feature column labels
  featureVectorLabelDataSet <- read.table(file = paste(dataRoot, "features.txt", sep=""), stringsAsFactors = FALSE)
  featureColumnLabels <- featureVectorLabelDataSet[, 2]
    
  # load the data sets
  train <- loadDataSet("train", featureColumnLabels, dataRoot)
  test <- loadDataSet("test", featureColumnLabels, dataRoot)
  
  #merge train and test dataSets together
  result <- rbind(train, test)
  
  #add activity names
  activityLabels <- read.table(file = paste(dataRoot, "activity_labels.txt", sep=""), col.names = c("Activity.ID", "Activity.Name"), stringsAsFactors = TRUE)
  result <- merge(result, activityLabels)
  
  # only select mean and std of each variable + activity names + subject ids
  result <- result[, c("Subject", "Activity.Name", 
                       "tBodyAcc-mean()-X", "tBodyAcc-mean()-Y", "tBodyAcc-mean()-Z", "tBodyAcc-std()-X", "tBodyAcc-std()-Y", "tBodyAcc-std()-Z", 
                       "tGravityAcc-mean()-X", "tGravityAcc-mean()-Y", "tGravityAcc-mean()-Z", "tGravityAcc-std()-X", "tGravityAcc-std()-Y", "tGravityAcc-std()-Z",
                       "tBodyAccJerk-mean()-X", "tBodyAccJerk-mean()-Y", "tBodyAccJerk-mean()-Z", "tBodyAccJerk-std()-X", "tBodyAccJerk-std()-Y", "tBodyAccJerk-std()-Z",
                       "tBodyGyro-mean()-X", "tBodyGyro-mean()-Y", "tBodyGyro-mean()-Z", "tBodyGyro-std()-X", "tBodyGyro-std()-Y", "tBodyGyro-std()-Z", 
                       "tBodyGyroJerk-mean()-X", "tBodyGyroJerk-mean()-Y", "tBodyGyroJerk-mean()-Z", "tBodyGyroJerk-std()-X", "tBodyGyroJerk-std()-Y", "tBodyGyroJerk-std()-Z",
                       "tBodyAccMag-mean()", "tBodyAccMag-std()", "tGravityAccMag-mean()", "tGravityAccMag-std()", 
                       "tBodyAccJerkMag-mean()", "tBodyAccJerkMag-std()", "tBodyGyroMag-mean()", "tBodyGyroMag-std()", "tBodyGyroJerkMag-mean()", "tBodyGyroJerkMag-std()", 
                       "fBodyAcc-mean()-X", "fBodyAcc-mean()-Y", "fBodyAcc-mean()-Z", 
                       "fBodyAcc-std()-X", "fBodyAcc-std()-Y", "fBodyAcc-std()-Z", 
                       "fBodyAccJerk-mean()-X", "fBodyAccJerk-mean()-Y", "fBodyAccJerk-mean()-Z", 
                       "fBodyAccJerk-std()-X", "fBodyAccJerk-std()-Y", "fBodyAccJerk-std()-Z", 
                       "fBodyGyro-mean()-X", "fBodyGyro-mean()-Y", "fBodyGyro-mean()-Z", 
                       "fBodyGyro-std()-X", "fBodyGyro-std()-Y", "fBodyGyro-std()-Z", 
                       "fBodyAccMag-mean()", "fBodyAccMag-std()", "fBodyBodyAccJerkMag-mean()", 
                       "fBodyBodyAccJerkMag-std()", "fBodyBodyGyroMag-mean()", "fBodyBodyGyroMag-std()", 
                       "fBodyBodyGyroJerkMag-mean()", "fBodyBodyGyroJerkMag-std()", "fBodyBodyGyroJerkMag-min()")]
  
  # sanitize column names from "-" and "()", which causes issues with ddapply bellow.
  colnames(result) <- c("Subject", "Activity.Name", 
                        "tBodyAcc.mean.X", "tBodyAcc.mean.Y", "tBodyAcc.mean.Z", "tBodyAcc.std.X", "tBodyAcc.std.Y", "tBodyAcc.std.Z", 
                        "tGravityAcc.mean.X", "tGravityAcc.mean.Y", "tGravityAcc.mean.Z", "tGravityAcc.std.X", "tGravityAcc.std.Y", "tGravityAcc.std.Z",
                        "tBodyAccJerk.mean.X", "tBodyAccJerk.mean.Y", "tBodyAccJerk.mean.Z", "tBodyAccJerk.std.X", "tBodyAccJerk.std.Y", "tBodyAccJerk.std.Z",
                        "tBodyGyro.mean.X", "tBodyGyro.mean.Y", "tBodyGyro.mean.Z", "tBodyGyro.std.X", "tBodyGyro.std.Y", "tBodyGyro.std.Z", 
                        "tBodyGyroJerk.mean.X", "tBodyGyroJerk.mean.Y", "tBodyGyroJerk.mean.Z", "tBodyGyroJerk.std.X", "tBodyGyroJerk.std.Y", "tBodyGyroJerk.std.Z",
                        "tBodyAccMag.mean", "tBodyAccMag.std", "tGravityAccMag.mean", "tGravityAccMag.std", 
                        "tBodyAccJerkMag.mean", "tBodyAccJerkMag.std", "tBodyGyroMag.mean", "tBodyGyroMag.std", "tBodyGyroJerkMag.mean", "tBodyGyroJerkMag.std", 
                        "fBodyAcc.mean.X", "fBodyAcc.mean.Y", "fBodyAcc.mean.Z", 
                        "fBodyAcc.std.X", "fBodyAcc.std.Y", "fBodyAcc.std.Z", 
                        "fBodyAccJerk.mean.X", "fBodyAccJerk.mean.Y", "fBodyAccJerk.mean.Z", 
                        "fBodyAccJerk.std.X", "fBodyAccJerk.std.Y", "fBodyAccJerk.std.Z", 
                        "fBodyGyro.mean.X", "fBodyGyro.mean.Y", "fBodyGyro.mean.Z", 
                        "fBodyGyro.std.X", "fBodyGyro.std.Y", "fBodyGyro.std.Z", 
                        "fBodyAccMag.mean", "fBodyAccMag.std", "fBodyBodyAccJerkMag.mean", 
                        "fBodyBodyAccJerkMag.std", "fBodyBodyGyroMag.mean", "fBodyBodyGyroMag.std", 
                        "fBodyBodyGyroJerkMag.mean", "fBodyBodyGyroJerkMag.std", "fBodyBodyGyroJerkMag.min")
  # lets summarize the data, by averaging on each of the variables
  require("plyr")
  result <- ddply(result, .(Subject, Activity.Name), summarise, 
                  Body.Acceleration.X.Mean = mean(tBodyAcc.mean.X),
                  Body.Acceleration.Y.Mean = mean(tBodyAcc.mean.Y),
                  Body.Acceleration.Z.Mean = mean(tBodyAcc.mean.Z),
                  Body.Acceleration.X.STD = mean(tBodyAcc.std.X),
                  Body.Acceleration.Y.STD = mean(tBodyAcc.std.Y),
                  Body.Acceleration.Z.STD = mean(tBodyAcc.std.Z),
                  Gravity.Acceleration.X.Mean = mean(tGravityAcc.mean.X),
                  Gravity.Acceleration.Y.Mean = mean(tGravityAcc.mean.Y),
                  Gravity.Acceleration.Z.Mean = mean(tGravityAcc.mean.Z),
                  Gravity.Acceleration.X.STD = mean(tGravityAcc.std.X),
                  Gravity.Acceleration.Y.STD = mean(tGravityAcc.std.Y),
                  Gravity.Acceleration.Z.STD = mean(tGravityAcc.std.Z),
                  Body.Acceleration.Jerk.X.Mean = mean(tBodyAccJerk.mean.X),
                  Body.Acceleration.Jerk.Y.Mean = mean(tBodyAccJerk.mean.Y),
                  Body.Acceleration.Jerk.Z.Mean = mean(tBodyAccJerk.mean.Z),
                  Body.Acceleration.Jerk.X.STD = mean(tBodyAccJerk.std.X),
                  Body.Acceleration.Jerk.Y.STD = mean(tBodyAccJerk.std.Y),
                  Body.Acceleration.Jerk.Z.STD = mean(tBodyAccJerk.std.Z),
                  Body.Gyroscope.X.Mean = mean(tBodyGyro.mean.X),
                  Body.Gyroscope.Y.Mean = mean(tBodyGyro.mean.Y),
                  Body.Gyroscope.Z.Mean = mean(tBodyGyro.mean.Z),
                  Body.Gyroscope.X.STD = mean(tBodyGyro.std.X),
                  Body.Gyroscope.Y.STD = mean(tBodyGyro.std.Y),
                  Body.Gyroscope.Z.STD = mean(tBodyGyro.std.Z),
                  Body.Gyroscope.Jerk.X.Mean = mean(tBodyGyroJerk.mean.X),
                  Body.Gyroscope.Jerk.Y.Mean = mean(tBodyGyroJerk.mean.Y),
                  Body.Gyroscope.Jerk.Z.Mean = mean(tBodyGyroJerk.mean.Z),
                  Body.Gyroscope.Jerk.X.STD = mean(tBodyGyroJerk.std.X),
                  Body.Gyroscope.Jerk.Y.STD = mean(tBodyGyroJerk.std.Y),
                  Body.Gyroscope.Jerk.Z.STD = mean(tBodyGyroJerk.std.Z),
                  Body.Acceleration.Mag.Mean = mean(tBodyAccMag.mean),
                  Body.Acceleration.Mag.STD = mean(tBodyAccMag.std),
                  Gravity.Acceleration.Mag.Mean = mean(tGravityAccMag.mean),
                  Gravity.Acceleration.Mag.STD = mean(tGravityAccMag.std),
                  Body.Acceleration.Jerk.Mag.Mean = mean(tBodyAccJerkMag.mean),
                  Body.Acceleration.Jerk.Mag.STD = mean(tBodyAccJerkMag.std),
                  Body.Gyroscope.Mag.Mean = mean(tBodyGyroMag.mean),
                  Body.Gyroscope.Mag.STD = mean(tBodyGyroMag.std),
                  Body.Gyroscope.Jerk.Mag.Mean = mean(tBodyGyroJerkMag.mean),
                  Body.Gyroscope.Jerk.Mag.STD = mean(tBodyGyroJerkMag.std),
                  Fourier.Body.Acceleration.Mean.X = mean(fBodyAcc.mean.X),
                  Fourier.Body.Acceleration.Mean.Y = mean(fBodyAcc.mean.Y),
                  Fourier.Body.Acceleration.Mean.Z = mean(fBodyAcc.mean.Z),
                  Fourier.Body.Acceleration.STD.X = mean(fBodyAcc.std.X),
                  Fourier.Body.Acceleration.STD.Y = mean(fBodyAcc.std.Y),
                  Fourier.Body.Acceleration.STD.Z = mean(fBodyAcc.std.Z),
                  Fourier.Body.Acceleration.Jerk.Mean.X = mean(fBodyAccJerk.mean.X),
                  Fourier.Body.Acceleration.Jerk.Mean.Y = mean(fBodyAccJerk.mean.Y),
                  Fourier.Body.Acceleration.Jerk.Mean.Z = mean(fBodyAccJerk.mean.Z),
                  Fourier.Body.Acceleration.Jerk.STD.X = mean(fBodyAccJerk.std.X),
                  Fourier.Body.Acceleration.Jerk.STD.Y = mean(fBodyAccJerk.std.Y),
                  Fourier.Body.Acceleration.Jerk.STD.Z = mean(fBodyAccJerk.std.Z),
                  Fourier.Body.Gyroscope.Mean.X = mean(fBodyGyro.mean.X),
                  Fourier.Body.Gyroscope.Mean.Y = mean(fBodyGyro.mean.Y),
                  Fourier.Body.Gyroscope.Mean.Z = mean(fBodyGyro.mean.Z),
                  Fourier.Body.Gyroscope.STD.X = mean(fBodyGyro.std.X),
                  Fourier.Body.Gyroscope.STD.Y = mean(fBodyGyro.std.Y),
                  Fourier.Body.Gyroscope.STD.Z = mean(fBodyGyro.std.Z),
                  Fourier.Body.Acceleration.Mag.Mean = mean(fBodyAccMag.mean),
                  Fourier.Body.Acceleration.Mag.STD = mean(fBodyAccMag.std),
                  Fourier.Body.Acceleration.Jerk.Mag.Mean = mean(fBodyBodyAccJerkMag.mean),
                  Fourier.Body.Acceleration.Jerk.Mag.STD = mean(fBodyBodyAccJerkMag.std),
                  Fourier.Body.Gyroscope.Mag.Mean = mean(fBodyBodyGyroMag.mean),
                  Fourier.Body.Gyroscope.Mag.STD = mean(fBodyBodyGyroMag.std),
                  Fourier.Body.Gyroscope.Jerk.Mag.Mean = mean(fBodyBodyGyroJerkMag.mean),
                  Fourier.Body.Gyroscope.Jerk.Mag.STD = mean(fBodyBodyGyroJerkMag.std))
  
  # write dataSet to text file
  if(!doNotWriteToFile) {
    write.table(result, file=resultFileName, row.names = FALSE)
  }
}

analysis <- runAnalysis()