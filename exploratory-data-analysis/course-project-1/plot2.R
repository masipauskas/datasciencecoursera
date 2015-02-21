run <- function() {
  startDate <- as.Date("2007-02-01")
  endDate <- as.Date("2007-02-02")
  dataSetFileName <- "household_power_consumption.txt.gz"

  data <- read.table(file = gzfile(dataSetFileName), header = TRUE, sep = ";", na.strings = "?", 
                     colClasses = c('character', 'character', rep('numeric', times = 7)));
  data$Time <- as.POSIXct(paste(data$Date, data$Time), format="%d/%m/%Y %H:%M:%S")
  data$Date <- as.Date(data$Date, format="%d/%m/%Y")
  
  dataset <- data[data$Date >= startDate & data$Date <= endDate, ]
  png("plot2.png")
  plot(dataset$Global_active_power ~ dataset$Time, ylab = "Global Active Power (kilowatts)", xlab = "", type = 'l')
  dev.off()
}

run()