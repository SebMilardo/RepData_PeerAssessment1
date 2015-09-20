---
title: "Reproducible Research: Peer Assessment 1"
output: 
html_document: 
keep_md: true
---


## Loading and preprocessing the data

```r
library(ggplot2)
library(chron)

# Load the data
setwd("~/R/RepData_PeerAssessment1")
data <- read.csv(unz("activity.zip", "activity.csv"))

# Process/transform the data (if necessary) into a format suitable for your analysis
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset

```r
# Ignore missing values
p <- na.omit(data)

# Calculate the total number of steps taken per day
steps_per_day <- aggregate(steps ~ date, data=p, FUN = sum)

# Make a histogram of the total number of steps taken each day
qplot(steps_per_day$steps,
      geom = "histogram",
      binwidth = 1000,
      xlab = "Steps per day"
)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
# Calculate and report the mean of the total number of steps taken per day
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
# Calculate and report the median of the total number of steps taken per day
median(steps_per_day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
average_steps <- aggregate(steps ~ interval, data=p, FUN = mean)
qplot(interval,
      steps,
      data = average_steps,
      geom = "line",
      xlab = "Interval",
      ylab = "Average number of steps taken"
)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
average_steps$interval[which.max(average_steps$steps)]
```

```
## [1] 835
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

```r
# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
sum(is.na(data))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. 

```r
# I chose to use the mean for that 5-minute interval

# Create a new dataset that is equal to the original dataset but with the missing data filled in
cData <- data

for (i in 1:nrow(cData)) {
     if (is.na(cData[i,1])){
        cData[i,1] <- average_steps$steps[average_steps$interval == cData$interval[i]] 
     }
}

# Make a histogram of the total number of steps taken each day and Calculate and report 
# the mean and median total number of steps taken per day.

# Calculate the total number of steps taken per day
csteps_per_day <- aggregate(steps ~ date, data=cData, FUN = sum)

# Make a histogram of the total number of steps taken each day
qplot(csteps_per_day$steps,
      geom = "histogram",
      binwidth = 1000,
      xlab = "Steps per day"
)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
# Calculate and report the mean of the total number of steps taken per day
mean(csteps_per_day$steps)
```

```
## [1] 10766.19
```

```r
# Calculate and report the median of the total number of steps taken per day
median(csteps_per_day$steps)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total daily number of steps?

The estimate for the mean is the same, while the median slightly increased. 

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

```r
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
# indicating whether a given date is a weekday or weekend day.
cData$day <- factor(ifelse(is.weekend(cData$date),"weekend","weekday"))

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval
# (x-axis) and the average number of steps taken, averaged across all weekday days or weekend 
# days (y-axis).
waverage_steps <- aggregate(cData$steps, by=list(interval = cData$interval, day = cData$day), FUN = mean)
ggplot(waverage_steps, aes(x=interval, y=x)) + facet_grid(day ~ .) + geom_line() + ylab("Average number of steps taken")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

