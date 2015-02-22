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
  
  # get the body accelaration vectors
  accelometerBodyAccelerationVectorXAxis <- util.getConsolidatedDataSet("BodyAcceleration.X", "body_acc_x", "Inertial Signals/")
  accelometerBodyAccelerationVectorYAxis <- util.getConsolidatedDataSet("BodyAcceleration.Y", "body_acc_y", "Inertial Signals/")
  accelometerBodyAccelerationVectorZAxis <- util.getConsolidatedDataSet("BodyAcceleration.Z", "body_acc_z", "Inertial Signals/")
  
  # get the angular velocity vectors
  accelometerAngularVelocityVectorXAxis <- util.getConsolidatedDataSet("AngularVelocity.X", "body_gyro_x", "Inertial Signals/")
  accelometerAngularVelocityVectorYAxis <- util.getConsolidatedDataSet("AngularVelocity.Y", "body_gyro_y", "Inertial Signals/")
  accelometerAngularVelocityVectorZAxis <- util.getConsolidatedDataSet("AngularVelocity.Z", "body_gyro_z", "Inertial Signals/")
  
  # get the total accelaration vectors
  accelometerTotalAccelerationVectorXAxis <- util.getConsolidatedDataSet("TotalAcceleration.X", "total_acc_x", "Inertial Signals/")
  accelometerTotalAccelerationVectorYAxis <- util.getConsolidatedDataSet("TotalAcceleration.Y", "total_acc_y", "Inertial Signals/")
  accelometerTotalAccelerationVectorZAxis <- util.getConsolidatedDataSet("TotalAcceleration.Z", "total_acc_z", "Inertial Signals/")
  
  # merge all results into a single data frame
  result <- cbind(subjectTable, activityTable, 
                  featureVectorDataSet, 
                  accelometerBodyAccelerationVectorXAxis, accelometerBodyAccelerationVectorYAxis, accelometerBodyAccelerationVectorZAxis,
                  accelometerAngularVelocityVectorXAxis, accelometerAngularVelocityVectorYAxis, accelometerAngularVelocityVectorZAxis,
                  accelometerTotalAccelerationVectorXAxis, accelometerTotalAccelerationVectorYAxis, accelometerTotalAccelerationVectorZAxis)
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
runAnalysis <- function(dataRoot = "", resultFileName = "analysis.txt") {
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
  
  # write dataSet to text file
  write.table(result, file=resultFileName, row.names = FALSE)
}

runAnalysis()