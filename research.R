if (!file.exists("./StormData.csv.bz2")) {
        fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(fileURL,destfile="./StormData.csv.bz2",method="curl")
}

stormData <- read.csv(bzfile('./StormData.csv.bz2'))

names(stormData)

stormData$YEAR <- as.numeric(format(as.Date(stormData$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))

stormLast10 <- subset(stormData,
                      stormData$YEAR >= 2001 & stormData$YEAR <= 2011,
                      select=c("STATE","EVTYPE","FATALITIES","INJURIES","PROPDMG",
                               "PROPDMGEXP","CROPDMG","CROPDMGEXP","YEAR"))

## free up memory after subsetting
rm(stormData)

library(dplyr)
library(ggplot2)

FatalityByType <- summarise(group_by(stormLast10,EVTYPE),FATALITIES = sum(FATALITIES))
FatalityByType <- FatalityByType[FatalityByType$FATALITIES > 0, ]
FatalityByType <- arrange(FatalityByType,desc(FATALITIES))
Top10Last10 <- head(FatalityByType,10)

## adjust Factors based on reduced dataset
## cast as Character to remove existing Factor Levels
Top10Last10$EVTYPE <- as.character(Top10Last10$EVTYPE)
## recast as Factor to add back applicable Factor Levels
Top10Last10$EVTYPE <- as.factor(Top10Last10$EVTYPE)
## reorder the Factor Levels based on the Fatality Numbers associated to the level
Top10Last10$EVTYPE <- reorder(Top10Last10$EVTYPE,Top10Last10$FATALITIES)
## reverse the order to be descending
Top10Last10$EVTYPE <- factor(Top10Last10$EVTYPE, levels=rev(levels(Top10Last10$EVTYPE)))

View(Top10Last10)

PlotLast10 <- qplot(Top10Last10$EVTYPE,Top10Last10$FATALITIES,geom="bar",stat="identity") 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
+ xlab("Weather Event") + scale_y_continuous("Total No. of Fatalities")
+ ggtitle("Fatalities By Weather Event Over Past 10 Years")

print(PlotLast10)

InjuryByType <- summarise(group_by(stormLast10,EVTYPE),INJURIES = sum(INJURIES))
InjuryByType <- InjuryByType[InjuryByType$INJURIES > 0, ]
InjuryByType <- arrange(InjuryByType,desc(INJURIES))
Top10Last10 <- head(InjuryByType,10)

## adjust Factors based on reduced dataset
## cast as Character to remove existing Factor Levels
Top10Last10$EVTYPE <- as.character(Top10Last10$EVTYPE)
## recast as Factor to add back applicable Factor Levels
Top10Last10$EVTYPE <- as.factor(Top10Last10$EVTYPE)
## reorder the Factor Levels based on the Fatality Numbers associated to the level
Top10Last10$EVTYPE <- reorder(Top10Last10$EVTYPE,Top10Last10$INJURIES)
## reverse the order to be descending
Top10Last10$EVTYPE <- factor(Top10Last10$EVTYPE, levels=rev(levels(Top10Last10$EVTYPE)))

View(Top10Last10)

PlotLast10 <- qplot(Top10Last10$EVTYPE,Top10Last10$INJURIES,geom="bar",stat="identity") 
+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
+ xlab("Severe Weather Type") + scale_y_continuous("Number of Injuries")
+ ggtitle("Injuries By Severe Weather Event Over Past 10 Years")

