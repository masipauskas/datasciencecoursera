NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# Filter the data, to only include the years 1999,2002,2005,2008
plotData <- NEI[NEI$year %in% c(1999, 2002, 2005, 2008), ]
plotData$Emissions = plotData$Emissions / 1000000
# Create a png file
png("analysis_Q1.png")
  # summarize the total emissions over year, and plot them as a barplot
  barplot(tapply(plotData$Emissions, plotData$year, FUN = sum), ylab = "Emissions (millions of tons)")
dev.off()