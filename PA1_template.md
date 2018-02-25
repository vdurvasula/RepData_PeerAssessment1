# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
#load activity data
unzip("./activity.zip")
activity_data <- read.csv("./activity.csv",sep=",")
#order data by date & steps
activity_data <- activity_data[order(activity_data$date, activity_data$steps), ]
#filter activity data without NA 
activity <- activity_data[!is.na(activity_data$steps), ]
```


## What is mean total number of steps taken per day?

```r
#calculate total number of steps taken per day 
activity_total <- activity %>% arrange(date, steps) %>% group_by(date) %>% summarise(sum = sum(steps))

#plot for Total number of steps per day
p <- ggplot(activity_total, aes(sum)) + 
     geom_histogram(bins = 30) +
     ggtitle("Total number of steps per day") +
     theme(plot.title = element_text(hjust = 0.5)) +
     labs(x="Total Steps")

# print plot
p
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# print Mean & Median on the console
print("mean and median of the total number of steps taken per day")
summary(activity_total$sum)[3:4]
```

```
## [1] "mean and median of the total number of steps taken per day"
##   Median     Mean 
## 10765.00 10766.19
```


## What is the average daily activity pattern?

```r
# calculate average daily activity 
activity_interval_mean <- activity %>% group_by(interval) %>% summarise(steps = mean(steps))

# plot average daily activity
p <- ggplot(activity_interval_mean, aes(x=interval, y=steps)) + 
     geom_line() +
     ggtitle("Average daily activity") +
     theme(plot.title = element_text(hjust = 0.5)) +
     labs(x="Interval",y="Average daily activity")

max_steps <- summary(activity_interval_mean$steps)[6]
max_activity<- activity_interval_mean[activity_interval_mean$steps==max_steps, ]

# plot maximum number of steps 
p <- p + geom_point(data = max_activity, aes(color="red",size=4)) +
     geom_label_repel(aes(x=interval, y=steps,label= ifelse(steps == max_steps,as.character(interval),""))) +
     theme(legend.position = "none")

# print plot
p
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# print interval for maximum number of steps on the console 
print(paste0("maximum number of steps at interval: ",max_activity$interval))
```

```
## [1] "maximum number of steps at interval: 835"
```


## Imputing missing values

Strategy followed to fill missing values is by calculating the mean number of steps calcuated for 5 minute interval


```r
# Process missing values by replacing NA's with mean number of steps 
activity_join <- left_join(activity_data, activity_interval_mean, by="interval")
na.steps <- which(is.na(activity_join$steps.x))

# print total number of missing values on the console
print (paste0("Total number for missing values: ", length(na.steps)))
```

```
## [1] "Total number for missing values: 2304"
```

```r
# replace missing values with mean number of steps per interval
activity_join$steps.x[na.steps] <- activity_join$steps.y[na.steps]
activity_impute <- activity_join[ ,c("steps.x","date","interval")]
names(activity_impute)[1] <- "steps" 

# calculate total number of steps taken per day 
activity_imputed_total <- activity_impute %>% arrange(date, steps) %>% group_by(date) %>% summarise(sum = sum(steps))

#plot Total number of steps per day
p <- ggplot() + 
     geom_histogram(data = activity_imputed_total, bins = 25, aes(sum, fill = "r")) +
     geom_histogram(data = activity_total,bins=25, aes(sum, fill = "b")) +
     ggtitle("Difference in total number of steps per day by Imputing") +
     labs(x="Total Steps") +
     theme(plot.title = element_text(hjust = 0.5)) +
     scale_fill_manual(name = "Group", labels = c("b" = "Before Imputing", "r" = "After Imputing"), 
                        values = c("b" = "blue", "r" = "red")) 
     
# print plot
p
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# print mean & median steps on the console
print("mean and median of the total number of steps taken per day")
```

```
## [1] "mean and median of the total number of steps taken per day"
```

```r
summary(activity_imputed_total$sum)[3:4]
```

```
##   Median     Mean 
## 10766.19 10766.19
```



## Are there differences in activity patterns between weekdays and weekends?

```r
# group activity data by weekday & weekend 
weekdays <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
activity_impute$wday <- factor(weekdays(as.Date(activity_impute$date)) %in% weekdays, levels=c(FALSE,TRUE), labels=c("Weekday","Weekend"))
# calculate mean activity 
activity_impute_mean <- activity_impute %>% group_by(interval,wday) %>% summarise(steps = mean(steps))
# plot activity patterns for weekdays and weekends
xyplot(steps ~ interval | levels(wday), 
           data = activity_impute_mean,
           type = "l",
           xlab = "Interval",
           ylab = "Number of steps",
           layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
