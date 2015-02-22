+++ "Coursera - UCI HAR Dataset analysis"

The script runs analysis on UCI HAR Dataset, by default it will expect the data being stored in "UCI HAR Dataset" directory, which should exist in your working directory.

This script depends on the following packages:
* plyr

If you would like to run the analysis on the different directory, re-run `runAnalysis` function with `dataRoot` - pointing to directory where, the data is located. And `resultFileName` pointing to location and file name of where to save the result.

Initial UCI HAR Dataset - contains a set of data of 30 subjects, being made to make different activities: 
* WALKING
* WALKING_UPSTAIRS
* WALKING_DOWNSTAIRS
* SITTING
* STANDING
* LAYING

The Android phone have taken a lot of timeseries measures on 50HZ frequency from Acellerometer and Gyroscope. Then further derived measures were arived by applying the Fast Fourier Transform on the initial measures. More data can be found at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The analyzed dataset - is effectivelly consolidated dataset, where all different time and Fast Fourier Transform have been grouped by the subject and activity. This is tidy data set, with each column representing different variable, and each row representing a different observation - each activity for each of the subjects.

After running the analysis, dataSet will be available in the environment in the user variable analysis.

If you would like to just load the dataSet, but not write to the data file, please declare `doNotWriteToFile <- TRUE` before Souring the script.