# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1. Load the data (i.e. read.csv())

```r
data <- read.csv(file = "./activity.csv", stringsAsFactors = FALSE)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

2. Process/transform the data (if necessary) into a format suitable for your analysis



## What is mean total number of steps taken per day?


1. Calculate the total number of steps taken per day

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
steps_table <- group_by(data, date) %>% summarize(steps = sum(steps, na.rm = TRUE))
head(steps_table)
```

```
## Source: local data frame [6 x 2]
## 
##         date steps
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
g <- ggplot(steps_table, aes(x = steps))
g <- g + geom_histogram()
g
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day

```r
print(mean(steps_table$steps))
```

```
## [1] 9354.23
```

```r
print(median(steps_table$steps))
```

```
## [1] 10395
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
interval_table <- group_by(data, interval) %>% summarize(steps = mean(steps, na.rm = TRUE))
g <- ggplot(interval_table, aes(x = interval, y = steps))
g <- g + geom_line()
g
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_steps_i <- which.max(interval_table$steps)
interval_table[max_steps_i,1]
```

```
## Source: local data frame [1 x 1]
## 
##   interval
## 1      835
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
#sum(is.na(data$date))
#sum(is.na(data$interval))
#No missing date or interval => number of NAs in the steps column = number of NAs
sum(is.na(data$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data_na <- data[is.na(data$steps),]
for (i in interval_table$interval) {
      data_na[data_na$interval == i,1] = interval_table[interval_table$interval == i,2]
}

head(data_na)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
data[is.na(data$steps),] <- data_na
sum(is.na(data$steps))
```

```
## [1] 0
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps_table_c <- group_by(data, date) %>% summarize(steps = sum(steps))
g <- ggplot(steps_table_c, aes(x = steps))
g <- g + geom_histogram()
g
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
length(unique(data$date))
```

```
## [1] 61
```

```r
print(mean(steps_table_c$steps))
```

```
## [1] 10766.19
```

```r
print(median(steps_table_c$steps))
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?


1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.



```r
data$workday <- ifelse(grepl("Sat|Sun", weekdays(as.Date(data$date), abbreviate = TRUE)), "Weekend", "Weekday")
head(data)
```

```
##       steps       date interval workday
## 1 1.7169811 2012-10-01        0 Weekday
## 2 0.3396226 2012-10-01        5 Weekday
## 3 0.1320755 2012-10-01       10 Weekday
## 4 0.1509434 2012-10-01       15 Weekday
## 5 0.0754717 2012-10-01       20 Weekday
## 6 2.0943396 2012-10-01       25 Weekday
```

```r
unique(data$workday)
```

```
## [1] "Weekday" "Weekend"
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
interval_table <- group_by(data, interval, workday) %>% summarize(steps = mean(steps, na.rm = TRUE))
head(interval_table)
```

```
## Source: local data frame [6 x 3]
## Groups: interval
## 
##   interval workday      steps
## 1        0 Weekday 2.25115304
## 2        0 Weekend 0.21462264
## 3        5 Weekday 0.44528302
## 4        5 Weekend 0.04245283
## 5       10 Weekday 0.17316562
## 6       10 Weekend 0.01650943
```

```r
g <- ggplot(interval_table, aes(x = interval, y = steps))
g <- g + geom_line()
g <- g + facet_grid(workday ~ .)
g
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 









