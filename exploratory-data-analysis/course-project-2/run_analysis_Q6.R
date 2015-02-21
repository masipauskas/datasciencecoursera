NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

library("gdata")
# Merge with cSS clasifiers, then filter the data, to only include the years between 1999 and 2008 EI. Sector to be motor related
plotData <- merge(NEI,SCC)
plotData <- plotData[plotData$year >= 1999 & plotData$year <= 2008 & plotData$fips %in% c("24510", "06037"), ]
plotData <- plotData[startsWith(plotData$EI.Sector, "Mobile", trim=TRUE, ignore.case=TRUE),]

library("ggplot2")

# Label facets to Baltimore City and Los Angeles County instead of numeric fips values
fips_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="fips") { 
    value[value=="24510"] <- "Baltimore City"
    value[value=="06037"]   <- "Los Angeles County"
  }
  return(value)
}

# Create a png file
ggplot(plotData,aes(x=year, weight=Emissions)) + geom_bar() + ylab("Emissions in tons") + ggtitle("Emissions from motor vehicles") + facet_grid(~fips, labeller=fips_labeller)
ggsave("analysis_Q6.png")