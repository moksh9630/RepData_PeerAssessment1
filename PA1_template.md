The file is downloaded from the web and the data set is read into R

    url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url,"activity.zip")
    unzip("activity.zip",exdir = "activity")
    activity <- read.csv("C:/Users/Sony/Documents/activity/activity.csv")

Calculation of Steps Per day and Histogram of steps per day
-----------------------------------------------------------

### Number of steps taken each day

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    activity1 <- group_by(activity,date)
    activity2 <- summarise(activity1, steps = sum(steps,na.rm = TRUE))
    activity2 <- as.data.frame(activity2)
    head(activity2)

    ##         date steps
    ## 1 2012-10-01     0
    ## 2 2012-10-02   126
    ## 3 2012-10-03 11352
    ## 4 2012-10-04 12116
    ## 5 2012-10-05 13294
    ## 6 2012-10-06 15420

### Histogram of number of steps taken each day

    hist(activity2$steps,xlab = "No. of Steps",ylab = "Frequency",main = "Histogram of Number of Steps Per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

### Mean and Median of total number of steps taken per day

    activity3 <- as.data.frame(summarise(activity1, steps_mean = mean(steps,na.rm = TRUE),steps_median = median(steps,na.rm = TRUE)))
    tail(activity3)

    ##          date steps_mean steps_median
    ## 56 2012-11-25   41.09028            0
    ## 57 2012-11-26   38.75694            0
    ## 58 2012-11-27   47.38194            0
    ## 59 2012-11-28   35.35764            0
    ## 60 2012-11-29   24.46875            0
    ## 61 2012-11-30        NaN           NA

    mean_steps <- mean(activity2$steps)
    median_steps <- median(activity2$steps)

The average steps taken per day is 9354.2295082 and the median of steps
taken per day is 10395

Average daily activity pattern
------------------------------

### Time series Plot

The below graph shows the Time series plot of Average Number of steps
taken for every 5 minute interval

    activity4 <- group_by(activity,interval)
    activity4 <- as.data.frame(summarise(activity4,steps = mean(steps,na.rm = TRUE)))
    plot(activity4$interval,activity4$steps,type = "l",xlab = "5 Min Interval",ylab = "Average No. of steps",main = "Time series Plot")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

### 5 Min interval with maximum number of steps

    interval <- activity4[activity4$steps == max(activity4$steps),]

The Interval 835 have the maximum number of steps i.e. 206.1698113

Imputing Missing Values
-----------------------

### Number of Missing values

    na_value <- sum(is.na(activity$steps))

The Number of Missing values in the steps column is 2304

### Strategy to fill the missing data and creating a new data set with missing values filled in

    activity4 <- group_by(activity,interval)
    activity4 <- as.data.frame(summarise(activity4,steps = mean(steps,na.rm = TRUE)))
    logi <- which(is.na(activity$steps))
    activity5 <- activity
    for(i in seq_along(logi)){
      interval1 <- activity5[logi[i],3]  
      activity5[logi[i],1] <- activity4[activity4$interval == interval1,2]
    }
    head(activity5)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

### Histogram of new data set comparison of mean and median with previous data set after filling in the missing values

    library(dplyr)
    activity6 <- group_by(activity5,date)
    activity7 <- as.data.frame(summarise(activity6, steps = sum(steps)))
    hist(activity7$steps,xlab = "No. of Steps",ylab = "Frequency",main = "Histogram of Number of Steps Per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    activity8 <- as.data.frame(summarise(activity6, steps_mean = mean(steps),steps_median = median(steps)))
    head(activity8)

    ##         date steps_mean steps_median
    ## 1 2012-10-01   37.38260     34.11321
    ## 2 2012-10-02    0.43750      0.00000
    ## 3 2012-10-03   39.41667      0.00000
    ## 4 2012-10-04   42.06944      0.00000
    ## 5 2012-10-05   46.15972      0.00000
    ## 6 2012-10-06   53.54167      0.00000

    mean_steps1 <- mean(activity7$steps)
    median_steps1 <- median(activity7$steps)

The Average number of steps taken per day after processing the missing
values is 1.076618910^{4} and the median number of steps taken per day
after processing the missing values is 1.076618910^{4} . The value of
average steps taken per day after processing the missing values has
increased by 1411.959171 and the median number of steps taken per day
after processing the missing values has increased by 371.1886792 .

Difference in Activity pattern on Weekdays and Weekends
-------------------------------------------------------

### Creating a new factor variable with two data sets = "weekday" and "Weekend"

    weekdays1 <- c('Monday','Tuesday','Wednesday','Friday','Saturday')
    activity5$Weekday <- factor(weekdays(as.POSIXct(activity5$date)) %in% weekdays1,levels = c(FALSE,TRUE),labels = c("weekend","weekday"))

### Creating a Panel Plot

    library(lattice)
    act_weekday <- activity5[activity5$Weekday %in% "weekday",]
    act_weekend <- activity5[activity5$Weekday %in% "weekend",]
    act_weekday <- act_weekday[,1:3]
    act_weekend <- act_weekend[,1:3]
    act_weekday1 <- group_by(act_weekday,interval)
    act_weekday1 <- as.data.frame(summarise(act_weekday1,steps = mean(steps)))
    act_weekend1 <- group_by(act_weekend,interval)
    act_weekend1 <- as.data.frame(summarise(act_weekend1,steps = mean(steps)))
    act_weekday1$Weekday <- "weekday"
    act_weekend1$Weekday <- "weekend"
    final_act <- rbind(act_weekday1,act_weekend1)
    xyplot(steps~interval | Weekday , data = final_act,type = "l",xlab = "Interval",ylab = "Number of steps",layout = c(1,2))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)
