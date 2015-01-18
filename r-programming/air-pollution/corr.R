corr <- function(directory, threshold = 0) {
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
  dataset <- dataset[complete.cases(dataset), ] # filter to only contain complete cases
  complete_cases <- ddply(dataset,.(ID), summarise, count = length(ID)) # summarize on number of complete cases
  
  complete_cases[complete_cases["ID"] > threshold,]
  
  result <- numeric()
  for (id in complete_cases$ID) {
    data = dataset[dataset["ID"] == id,]
    result <- append(cor(x = data$sulfate, y = data$nitrate))
  }
  result
}