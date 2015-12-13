NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# Filter the data, to only include the years between 1999 and 2008 and location to be baltimore city
plotData <- NEI[NEI$year >= 1999 & NEI$year <= 2008 & NEI$fips == 24510, ]

# Normalize emissions to thousands of tons
plotData$Emissions = plotData$Emissions / 1000
# Create a png file
png("analysis_Q2.png")
# summarize the total emissions over year, and plot them as a barplot
barplot(tapply(plotData$Emissions, plotData$year, FUN = sum), ylab = "Emissions (thousands of tons)")
dev.off()