NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# Merge with cSS clasifiers, then filter the data, to only include the years between 1999 and 2008 EI. Sector to be Fuel Comb - Electric Generation - Coal
plotData <- merge(NEI,SCC)
plotData <- plotData[plotData$year >= 1999 & plotData$year <= 2008,]
plotData <- plotData[plotData$EI.Sector %in% c("Fuel Comb - Comm/Institutional - Coal", "Fuel Comb - Industrial Boilers, ICEs - Coal", "Fuel Comb - Electric Generation - Coal"),]

# Reduce pecission for graphing to millions of tonnes
plotData$Emissions = plotData$Emissions / 1000000

library("ggplot2")

# Create a png file
ggplot(plotData,aes(x=year, weight=Emissions)) + geom_bar() + ylab("Emissions (millions of tons)") + ggtitle("Coal combustion related emissions across United States")
ggsave("analysis_Q4.png")