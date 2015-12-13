run <- function() {
  startDate <- as.Date("2007-02-01")
  endDate <- as.Date("2007-02-02")
  dataSetFileName <- "household_power_consumption.txt.gz"
  
  data <- read.table(file = gzfile(dataSetFileName), header = TRUE, sep = ";", na.strings = "?", 
                     colClasses = c('character', 'character', rep('numeric', times = 7)));
  data$Time <- as.POSIXct(paste(data$Date, data$Time), format="%d/%m/%Y %H:%M:%S")
  data$Date <- as.Date(data$Date, format="%d/%m/%Y")
  
  dataset <- data[data$Date >= startDate & data$Date <= endDate, ]
  png("plot3.png")
  plot(dataset$Sub_metering_1 ~ dataset$Time, ylab = "Energy sub metering", xlab = "", col="black", type = 'l')
  lines(dataset$Sub_metering_2 ~ dataset$Time, col = "red")
  lines(dataset$Sub_metering_3 ~ dataset$Time, col = "blue")
  legend('topright', c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), col=c('black', 'red', 'blue'), lwd=1)
  dev.off()
}

run()