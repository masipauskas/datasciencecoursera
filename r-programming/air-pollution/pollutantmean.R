pollutantmean <- function(directory, pollutant, id = 1:332) {
  import_csv <- function(directory, pattern = "*.csv") {
    files = list.files(directory, pattern = "*.csv", full.names = TRUE)
    
    for (index in 1:length(files)) {
      if (index == 1) { # create initial data frame and list of columns
        data <- read.csv(files[index])
      } else { # append to existing data frame
        d <- read.csv(files[index])
        data<-rbind(data, d)
      }
    }
    
    data
  }
  
  data <- import_csv(directory)
  round(mean(subset(data, ID %in% id)[,pollutant], na.rm = TRUE), digits = 3)
}

pollutantmean_test <- function() {
  directory <- "specdata"
  success <- TRUE
  r <- pollutantmean(directory, "sulfate", 1:10)
  if (r != 4.064) {
    print ('pollutantmean("specdata", "sulfate", 1:10) -> r != 4.064')
    success <- FALSE
  }

  r <- pollutantmean(directory, "nitrate", 70:72)
  if (r != 1.706) {
    print ('pollutantmean("specdata", "nitrate", 70:72) -> r != 1.706')
    success <- FALSE
  }
  
  r <- pollutantmean(directory, "nitrate", 23)
  if (r != 1.281) {
    print ('pollutantmean("specdata", "nitrate", 23) -> r != 1.281')
    success <- FALSE
  }
  
  stopifnot(success)
}