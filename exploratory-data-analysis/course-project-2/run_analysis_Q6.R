NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

library("gdata")
# Merge with cSS clasifiers, then filter the data, to only include the years between 1999 and 2008 EI. Sector to be Fuel Comb - Electric Generation - Coal
plotData <- merge(NEI,SCC)
plotData <- plotData[plotData$year >= 1999 & plotData$year <= 2008 & plotData$fips == "24510", ]
plotData <- plotData[startsWith(plotData$EI.Sector, "Mobile", trim=TRUE, ignore.case=TRUE),]

library("ggplot2")

# Create a png file
ggplot(plotData,aes(x=year, weight=Emissions)) + geom_bar() + ylab("Emissions in tons") + ggtitle("Emissions from motor vehicles")
ggsave("analysis_Q6.png")