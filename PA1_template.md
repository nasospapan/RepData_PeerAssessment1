# Activity Data - Week 2 PA
Nasos Papanikolaou  
May 4, 2017  


Libraries

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.3
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.3.3
```

## Loading and Processing the data

1. Load the data: Since the data is in a csv file, I am using read.csv to load
it to a dataframe called activity. This assumes that the data set is already in
the current directory.


```r
if(!file.exists('activity.csv')){
      unzip('activity.zip')
}

activity <- read.csv('activity.csv', na.strings = "NA")
```

2. Process/transform the data (if necessary) into a format suitable for your
analysis: When I loaded the data, the date variable was loaded as a factor, so
here I convert it to a date class, using as.Date.


```r
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?
For this part of the assignment, I will ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day: Here I use the aggregate
function to create a new dataframe, total_steps_per_day, which shows the total
number of steps for every day in the data. Note that the aggregate function 
ignores missing values as a default.

```r
total_steps_per_day <- aggregate(steps ~ date, data = activity, FUN = "sum")
```

2. Make a histogram of the total number of steps taken each day

```r
hist(total_steps_per_day$steps, breaks = seq(0, 25000, by = 2000), 
     xlab = "Total Steps/Day in bins of 2000", main = "Histogram of Total Steps/Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken 
per day. Here I am using the mean and median functions.

```r
mean_total_steps_per_day <- mean(total_steps_per_day$steps)
median_total_steps_per_day <- median(total_steps_per_day$steps)
```

- The mean total steps per day is: 

```r
mean_total_steps_per_day
```

```
## [1] 10766.19
```
- The median total steps per day is: 

```r
median_total_steps_per_day
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all days (y-axis). Here
I first calculate the average steps per 5 min interval over all days in the data
set and then I am using qplot to graph the time series.

```r
avg_steps_per_interval <- aggregate(steps ~ interval, data = activity, FUN = "mean")
qplot(x = interval, y = steps, data = avg_steps_per_interval, geom = "line",
      xlab = "5 min intervals in a day", ylab = "Avg Steps/Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_step_interval <- avg_steps_per_interval$interval[avg_steps_per_interval$steps == max(avg_steps_per_interval$steps)]
format(strptime(sprintf("%04d", max_step_interval), format = "%H%M"), format = "%I:%M%p")
```

```
## [1] "08:35AM"
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs):

```r
sum(is.na(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The
strategy I am using is to replace every missing value with the average steps of
the 5 min interval over all the days.

First, create a new dataframe by joining the initial dataset to the averages per
5 min interval that we calculated in the previous section. Then I correct the name
of the variable steps that corresponds to the average steps of the intervals.
Then I create a logical vector that I will use for indexing when I replace the
missing values with the averages.

```r
narm_activity <- join(activity, avg_steps_per_interval, by = "interval")
names(narm_activity)[4] <- "avgsteps"
u <- is.na(narm_activity$steps)
narm_activity$steps[u] <- narm_activity$avgsteps[u]
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
my_activity <- narm_activity[,c("steps", "date", "interval")]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? Here I repeat the calculations
from the earlier section, but using the new dataset.


```r
new_total_steps_per_day <- aggregate(steps ~ date, data = my_activity, FUN = "sum")

hist(new_total_steps_per_day$steps, breaks = seq(0, 25000, by = 2000), 
     xlab = "Total Steps/Day in bins of 2000", main = "Histogram after removing missing values")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
mean_new_total_steps_per_day <- mean(new_total_steps_per_day$steps)
median_new_total_steps_per_day <- median(new_total_steps_per_day$steps)
```
- The new mean is:

```r
mean_new_total_steps_per_day
```

```
## [1] 10766.19
```
- The new median is:

```r
median_new_total_steps_per_day
```

```
## [1] 10766.19
```

The mean has remained the same, because I used the 5 min average over all days
to replace the missing values. However, the median has changed, due to these new
entries. Had I used the median as the replacement of missing values, then the
median would not have changed, but the mean would have.

Imputing the missing values has added more data into the calculations of total steps
or average steps. As a result for example, the histogram frequencies have changed.
The changes reflect the increased number of data due to the replacement of missing
values. So, whichever bin had a missing value before, now has an increased number
of total steps.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
my_activity$day <- ifelse(weekdays(my_activity$date) == "Saturday" | weekdays(my_activity$date) == "Sunday", "weekend", "weekday")
my_activity$day <- as.factor(my_activity$day)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
avgsteps <- aggregate(steps ~ interval + day, data = my_activity, FUN = "mean")

g <- ggplot(avgsteps, aes(interval, steps))

g <- g + geom_line(color = "red") + labs(x = "Intervals", y = "Avg Steps") + 
      facet_wrap(~day, nrow = 2)
g
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
