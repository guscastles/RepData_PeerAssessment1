---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

```r
data = read.csv("activity.csv")
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?

```r
library(dplyr)
```


```r
actual_data <- data %>%
               select(steps, date, interval) %>%
               filter(!is.na(steps))
dt_agg <- aggregate(steps ~ date, actual_data, sum)
with(dt_agg,  hist(dt_agg$steps, 
                   breaks = 10, 
                   xlab = "Number of Steps", 
                   main = "Total Number of Steps per Day"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

#### Median and Mean Values  

```r
with(dt_agg, {
        boxplot(dt_agg$steps)
        abline(h = mean(dt_agg$steps), lwd = 1, col = "red")
})
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
cat(paste(summary(dt_agg)[, 2][4],
      summary(dt_agg)[, 2][3],
      sep = "\n"))
```

```
## Mean   :10766  
## Median :10765
```
## What is the average daily activity pattern?

```r
dt_avg <- aggregate(steps ~ interval, data = actual_data, mean)
with(dt_avg, plot(y = steps, 
                  x = interval, 
                  type = "l", 
                  ylab = "Average Number of Steps per Day",
                  xlab = "5-minute Interval Through the Day")
)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
max_avg_steps <-dt_avg %>%
                select(steps, interval) %>%
                filter(steps == max(steps))
```
The 5-minute interval that contains the maximum average of steps, across all days, is 835, with 206.2 steps.

## Imputing missing values

```r
missing_values <- sum(is.na(data$steps))
```
The number of rows with missing values is 2304.


## Are there differences in activity patterns between weekdays and weekends?
