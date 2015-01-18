complete <- function(directory, id = 1:332) { 
  require(plyr)
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
  
  dataset <- import_csv(directory)
  dataset <- subset(dataset, ID %in% id) # filter by id's
  dataset <- dataset[complete.cases(dataset), ] # filter to only contain complete cases
  result <- ddply(dataset,.(ID),
                  summarise,
                  count = length(ID))
  names(result) <- c ("id", "nobs") #rename columns
  result
}