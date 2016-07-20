# Code for Reproducible Research Assignment 1
# Written by khychan on 17 JUL 2016

## initialise libraries.
library(ggplot2)

## Code for reading in the dataset.
file.name <- "activity.csv"
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
DataZip <- "./activity.zip"

if (!file.exists(file.name)) {
        download.file(url,destfile = DataZip)
        unzip(DataZip)
        file.remove(DataZip)
}
data <- read.csv("activity.csv",header=TRUE)

## Histogram of the total number of steps taken each day.
TotalStepsPerDay <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)

PlotA <- qplot(TotalStepsPerDay, binwidth=1000, fill=I("blue"), xlab="Total Number of Steps Each Day", ylab="Frequency", main="Histogram of the total steps taken each day")
print(PlotA)

## Mean and median number of steps taken each day.

MeanStepsPerDay <- mean(TotalStepsPerDay, na.rm=TRUE)
MedianStepsPerDay <- median(TotalStepsPerDay, na.rm=TRUE)

## Time series plot of the average number of steps taken.

AverageSteps <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)
PlotB <- ggplot(data=AverageSteps, aes(x=interval, y=steps))+
                geom_line()+
                xlab("5-Minute interval")+
                ylab("Average number of steps taken")+
                ggtitle("Time series plot of the average number of steps taken")
print(PlotB)

## The 5 minute interval that contains the highest average number of steps.

IntervalWithMostStepsOnAverage <- AverageSteps[which.max(AverageSteps$steps), ]

## Code to describe and show a strategy for imputing missing data.
### First find the missing data.
MissingData <- is.na(data$steps)
print(table(MissingData))

### Replace the missing data with the average within that 5 minute interval.

ReplaceValue <- function(steps, interval) {
        filler <- NA
        if (!is.na(steps))
                filler <- c(steps)
        else
                filler <- (AverageSteps[AverageSteps$interval==interval, "steps"])
        return(filler)
}
FilledData <- data
FilledData$steps <- mapply(ReplaceValue, FilledData$steps, FilledData$interval)

## Histogram of the total number of steps taken each day after missing values are imputed.

TotalSteps <- tapply(FilledData$steps, FilledData$date, FUN=sum)
PlotC <- qplot(TotalSteps, binwidth=1000, fill=I("blue"), xlab="Total Number of Steps Each Day", ylab="Frequency", main="Histogram of the total steps taken each day")
print(PlotC)

## Mean and median number of steps taken each day after missing values are imputed.

MeanStepsPerDayFilledData <- mean(TotalSteps)
MedianStepsPerDayFilledData <- median(TotalSteps)

## Function that returns whether the date supplied is a weekday or weekend.
WeekdayOrWeekend <- function(date){
        day <- weekdays(date)
        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
                return("Weekday")
        if (day %in% c("Saturday", "Sunday"))
                return("Weekend")
}
FilledData$date <- as.Date(FilledData$date)
FilledData$day <- sapply(FilledData$date, FUN=WeekdayOrWeekend)

## Panel plot comparing the average steps taken per 5 minute interval across weekdays and weekends.
AverageStepsByDay <- aggregate(steps ~ interval + day, data=FilledData, mean)
PlotD <- ggplot(data=AverageStepsByDay, aes(interval, steps))+
        geom_line()+
        facet_grid(day ~ .)+
        facet_wrap( ~ day, ncol=1)+
        xlab("5-Minute interval")+
        ylab("Average number of steps taken")+
        ggtitle("Time series plot of the average number of steps taken")
print(PlotD)

