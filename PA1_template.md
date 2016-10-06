    # Clear Variables
    rm(list = ls())

    # Attach packages
    library(plyr)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:plyr':
    ## 
    ##     here

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    library(ggplot2)

Loading and preprocessing the data
==================================

### Show any code that is needed to

1.  Load the data (i.e. read.csv())

<!-- -->

    # Create data folder to store files
    if(!file.exists("./data")){dir.create("./data")}

    # Download raw data
    fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileURL, destfile="./data/activity.zip")

    # Unzip file
    unzip(zipfile="./data/activity.zip", exdir="./data")

    # Read in file(s)
    activity <- read.csv("./data/activity.csv")

    # Review data
    dim(activity)

    ## [1] 17568     3

    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    tail(activity)

    ##       steps       date interval
    ## 17563    NA 2012-11-30     2330
    ## 17564    NA 2012-11-30     2335
    ## 17565    NA 2012-11-30     2340
    ## 17566    NA 2012-11-30     2345
    ## 17567    NA 2012-11-30     2350
    ## 17568    NA 2012-11-30     2355

    summary(activity)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

    names(activity)

    ## [1] "steps"    "date"     "interval"

    str(activity)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

1.  Process/transform the data (if necessary) into a format suitable for
    your analysis

<!-- -->

    # Total steps by date, remove NA
    total.steps <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE)

    # Format date
    activity$date <- ymd(activity$date)

What is mean total number of steps taken per day?
=================================================

    mean(total.steps)

    ## [1] 9354.23

*For this part of the assignment, you can ignore the missing values in
the dataset.*

1.  Calculate the total number of steps taken per day

<!-- -->

    stepsPerDay <- activity %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarize(steps = sum(steps)) %>%
        print

    ## # A tibble: 53 × 2
    ##          date steps
    ##        <date> <int>
    ## 1  2012-10-02   126
    ## 2  2012-10-03 11352
    ## 3  2012-10-04 12116
    ## 4  2012-10-05 13294
    ## 5  2012-10-06 15420
    ## 6  2012-10-07 11015
    ## 7  2012-10-09 12811
    ## 8  2012-10-10  9900
    ## 9  2012-10-11 10304
    ## 10 2012-10-12 17382
    ## # ... with 43 more rows

1.  Make a histogram of the total number of steps taken each day

<!-- -->

    plot1 <- ggplot(stepsPerDay, aes(date, steps/1000))
    plot1 + geom_bar(stat="identity") + 
        labs(x = "", y = "Steps (thousand)") +
        labs(title= "Total Number of Steps Taken Each Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

1.  Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

    mean(total.steps)

    ## [1] 9354.23

    median(total.steps)

    ## [1] 10395

What is the average daily activity pattern?
===========================================

    stepsPerInterval <- activity %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) %>%
        print

    ## # A tibble: 288 × 2
    ##    interval     steps
    ##       <int>     <dbl>
    ## 1         0 1.7169811
    ## 2         5 0.3396226
    ## 3        10 0.1320755
    ## 4        15 0.1509434
    ## 5        20 0.0754717
    ## 6        25 2.0943396
    ## 7        30 0.5283019
    ## 8        35 0.8679245
    ## 9        40 0.0000000
    ## 10       45 1.4716981
    ## # ... with 278 more rows

1.  Make a time series plot (i.e. type = "l") of the 5-minute
    interval (x-axis) and the average number of steps taken, averaged
    across all days (y-axis)

<!-- -->

    plot(stepsPerInterval, type = "l", xlab = "Interval", ylab = "Average Steps")
    title(main = "Average Steps Taken by Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    stepsPerInterval[which.max(stepsPerInterval$steps),]$interval

    ## [1] 835

Imputing missing values
=======================

*Note that there are a number of days/intervals where there are missing
values (coded as NA). The presence of missing days may introduce bias
into some calculations or summaries of the data.*

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)

<!-- -->

    # Total number of missing values (i.e. NA) in activity
    sum(is.na(activity))

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in
    the dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

        Using dplyr's mutate function, we'll change all NA values to the mean of all steps.

2.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- -->

    # Create dataset with NA changed to mean of all steps
    activityNAtoMean <- activity %>%
        group_by(interval) %>%
        mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
    summary(activityNAtoMean)

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0

1.  Make a histogram of the total number of steps taken each day

<!-- -->

    activityNAtoMean.steps <- activityNAtoMean %>%
        group_by(date) %>%
        summarize(steps = sum(steps))
        
    ggplot(activityNAtoMean.steps, aes(x=date, y=steps)) +
        geom_bar(stat = "identity") +
        labs(x = "", y = "Steps (Imputed)", title = "Total Steps by Date (Imputed)")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-13-1.png)

-   ...and Calculate and report the mean and median total number of
    steps taken per day.

<!-- -->

    imputed.steps <- tapply(activityNAtoMean$steps, activityNAtoMean$date, FUN = sum, na.rm = TRUE)
    activityNAtoMean$date <- ymd(activityNAtoMean$date)
    mean(imputed.steps)

    ## [1] 10766.19

    median(imputed.steps)

    ## [1] 10766.19

-   Do these values differ from the estimates from the first part of the
    assignment?

<!-- -->

    if(mean(total.steps)==mean(imputed.steps)) {
        "No, mean values are equal."
    }   else {
        "Yes, mean values differ."
    }

    ## [1] "Yes, mean values differ."

    if(median(total.steps)==median(imputed.steps)) {
        "No, median values are equal." 
    }   else {
        "Yes, median values differ."
    }

    ## [1] "Yes, median values differ."

-   What is the impact of imputing missing data on the estimates of the
    total daily number of steps?

Imputing the missing data impacted the estimates by the following
differences:

    summary(imputed.steps) - summary(total.steps)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    3041     370    1416       0       0

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

*For this part the weekdays() function may be of some help here. Use the
dataset with the filled-in missing values for this part.*

1.  Create a new factor variable in the dataset with two levels -
    "weekday" and "weekend" indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    weekdayWeekend <- function(date){
        if(weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
            "weekend"
        }else {
            "weekday"
        }
    }

    activity$daytype <- as.factor(sapply(activity$date, weekdayWeekend))
    summary(activity)

    ##      steps             date               interval         daytype     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   weekday:12960  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   weekend: 4608  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5                  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0                  
    ##  NA's   :2304

1.  Make a panel plot containing a time series plot (i.e. type = "l") of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

<!-- -->

    par(mfrow = c(2,1))

    stepsWeekday <- aggregate(steps ~ interval, data = activity, 
        subset = activity$daytype == "weekday", FUN = mean)
    stepsWeekend <- aggregate(steps ~ interval, data = activity, 
        subset = activity$daytype == "weekend", FUN = mean)

    plot(stepsWeekday, type = "l", main = "Weekday")
    plot(stepsWeekend, type = "l", main = "Weekend")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-18-1.png)
