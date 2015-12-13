NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# Filter the data, to only include the years between 1999 and 2008 and location to be baltimore city
plotData <- NEI[NEI$year >= 1999 & NEI$year <= 2008 & NEI$fips == 24510, ]

library("ggplot2")

# Create a png file
ggplot(plotData,aes(x=year, weight=Emissions)) + geom_bar() + facet_grid(~type) + ylab("Emissions in tons")
ggsave("analysis_Q3.png")