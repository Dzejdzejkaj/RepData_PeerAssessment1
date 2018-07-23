---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

# Reproducible Research: Peer-graded Assignment: Course Project 1 
## Loading and preprocessing the data
1. Load the data (i.e. type = "l")

```r
path <- "/Users/Jana/Documents/Data Analysis/R Programming Exercises/Reproducible-Research/Week2"
setwd(path)
getwd()
```

```
## [1] "/Users/Jana/Documents/Data Analysis/R Programming Exercises/Reproducible-research/Week2"
```

```r
data <- read.csv("activity.csv", colClasses = c("numeric", "character", 
    "numeric"))
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
date <- data$date
date <- as.Date(as.character(date), "%Y%m%d")
```

## What is mean total number of steps taken per day?
*For this part of the assignment, you can ignore the missing values in the dataset.*

1. Calculate the total number of steps taken per day

```r
stepsPerDay <- aggregate(steps ~ date, data = data, FUN = sum, na.rm = TRUE)
```

2. Make a histogram of the total number of steps taken each day

```r
hist(stepsPerDay$steps, main = "Total number of steps taken each day", xlab = "day", ylab = "number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(stepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
meanPerDay <- with(data, tapply(steps, date, mean, na.rm = TRUE))
meanPerInterval <- with(data, tapply(steps, interval, mean, na.rm = TRUE))
interval <- data$interval
```

1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot(row.names(meanPerInterval), meanPerInterval, type = "l", col = "red", main = "Average number of steps taken", xlab = "Interval", ylab = "Average number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxInterval <- which.max(meanPerInterval)
print(maxInterval)
```

```
## 835 
## 104
```

## Imputing missing values
*Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.*

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NAs <- sum(is.na(data))
print(NAs)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  
*I will replace the missing value with the mean of that interval and create a new data set called data2*

```r
meanPerInterval <- aggregate(steps ~ interval, data, FUN = mean, na.rm = TRUE)
data2 <- data
for (i in 1:nrow(data2)) {
    if (is.na(data2$steps[i] == TRUE)) {
        data2$steps[i]<-meanPerInterval$steps[match(data2$interval[i],meanPerInterval$interval)]
    }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
meanPerDay2 <- aggregate(steps ~ date, data2, FUN = mean, na.rm = TRUE)
meanPerInterval2 <- aggregate(steps ~ interval, data2, FUN = mean, na.rm = TRUE)
stepsPerDay2 <- aggregate(steps ~ date, data2, FUN = sum, na.rm = TRUE)

hist(stepsPerDay2$steps, main = "Total number of steps taken per day", xlab = "Day", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
mean(stepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
mean(stepsPerDay2$steps) # There is minimal impact when NAs are replaced by mean per day
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$steps)
```

```
## [1] 10765
```

```r
median(stepsPerDay2$steps) # There is minimal impact when NAs are replaced by mean per day
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
*For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.*

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
data2$date <- as.Date(data2$date)
data2$date <- weekdays(data2$date)

for (i in 1:nrow(data2)) {
    if (data2$date[i] == "Saturday" | data2$date[i] == "Sunday") {
        data2$date[i] <- "Weekend"
    } else {
        data2$date[i] <- "Weekday"
    }
}
```

2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
stepsPerInterval <- aggregate(steps ~ interval + date, data2, FUN = mean, na.rm = TRUE)

library(lattice)
xyplot(steps ~ interval | date, stepsPerInterval, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
