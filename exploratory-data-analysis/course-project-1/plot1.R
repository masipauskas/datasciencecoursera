run <- function() {
  startDate <- as.Date("2007-02-01")
  endDate <- as.Date("2007-02-02")
  dataSetFileName <- "household_power_consumption.txt.gz"
  
  setAs("character","dmyDateFormat", function(from) as.Date(from, format="%d/%m/%Y"))
  data <- read.table(file = gzfile(dataSetFileName), header = TRUE, sep = ";", na.strings = "?", 
             colClasses = c('dmyDateFormat', 'character', rep('numeric', times = 7)));
  
  dataset <- data[data$Date >= startDate & data$Date <= endDate, ]
  png("plot1.png")
  hist(dataset$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col = "#ff2500")
  dev.off()
}

run()