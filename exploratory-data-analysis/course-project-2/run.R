NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

plotData <- NEI[year %in% c(1999, 2002, 2005, 2008), ]
