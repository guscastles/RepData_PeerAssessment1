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
total_steps <- function(dataset) {
        aggregate(steps ~ date, dataset, sum)
}


non_missing_data <- function(dataset) {
        dataset %>%
               select(steps, date, interval) %>%
               filter(!is.na(steps))
}

plot_histogram <- function(dataset) {
        dt_agg <- total_steps(dataset)
        with(dt_agg,  hist(steps, 
                           breaks = 10, 
                           xlab = "Number of Steps", 
                           main = "Mean Total Number of Steps per Day"))
}
actual_data <- non_missing_data(data)
plot_histogram(actual_data)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

#### Median and Mean Values  

```r
plot_mean_and_median <- function(dataset) {
        with(dataset, {
                boxplot(steps, ylab = "Number of Steps", main = "Mean Total Number of Steps")
                abline(h = mean(steps), lwd = 1, col = "red")
        })
}

plot_mean_and_median(total_steps(actual_data))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
show_mean_and_median <- function(dataset) {
        cat(paste(summary(total_steps(dataset))[, 2][4],
              summary(total_steps(dataset))[, 2][3],
              sep = "\n"))
}

show_mean_and_median(actual_data)
```

```
## Mean   :10766  
## Median :10765
```
## What is the average daily activity pattern?

```r
dt_avg <- aggregate(steps ~ interval, data = actual_data, mean)
with(dt_avg, {plot(y = steps, 
                  x = interval, 
                  type = "l", 
                  ylab = "Average Number of Steps",
                  xlab = "5-minute Interval per Day")
              title("Average Daily Steps per Interval")
     }
)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
max_avg_steps <- dt_avg %>%
                 select(steps, interval) %>%
                 filter(steps == max(steps))
```
The 5-minute interval that contains the maximum average of steps, across all days, is 835, with 206.2 steps.

## Imputing missing values

```r
missing_values <- sum(is.na(data$steps))
```
The number of rows with missing values is 2304.

The missing values will be replaced by the average value of a given 5-minute interval.

```r
data[is.na(data$steps), "steps"] <- sapply(data[is.na(data$steps), "interval"],
        function(interval) {dt_avg[dt_avg["interval"] == interval, "steps"]})
head(data)
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
The new histogram for the total number of steps is

```r
plot_histogram(data)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

With new median and mean valus

```r
plot_mean_and_median(total_steps(data))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
show_mean_and_median(data)
```

```
## Mean   :10766  
## Median :10766
```
Comparatively, there is a difference of 0.009% only between the median value. According to this strategy the impact is minimum to none.

## Are there differences in activity patterns between weekdays and weekends?

A new factor variable *weekday* will provide the information needed by this question

```r
day_type <- function(dt) {
        if (weekdays(as.Date(dt)) %in% c("Saturday", "Sunday"))
                "weekend"
        else
                "weekday"
}

new_data <- data %>% mutate(weekday = factor(sapply(date, day_type)))
head(new_data)
```

```
##       steps       date interval weekday
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

The following plot shows that, on weekends, there is a more uniform distribution of the steps taken, whilst the highest number of steps happens during weekdays.

```r
library(lattice)
```


```r
new_avg_data <- aggregate(steps ~ interval + weekday, data = new_data, mean)
xyplot(steps ~ interval | weekday, data = new_avg_data, layout = c(1, 2), type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
