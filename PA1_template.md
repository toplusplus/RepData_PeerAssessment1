# Lifelogging
@toplusplus  
August 1, 2017

Overview
--------

There is an increasing use of technology to collect data about oneself.
Smartphone apps, GPS devices, physical activity trackers, all of these
technologies allow individuals to track aspects of their daily lives
like daytime activity, number of steps, amount of sleep, heart rate and
even mood. People who track themselves regularly think that data
collected may help them take action to improve their lives. See more at:
[Quantified Self
Movement](https://en.wikipedia.org/wiki/Quantified_Self).

This document expose an **exploratory data analysis** using data from a
personal activity monitoring device. This device collects data at 5
minute intervals through out the day.

About the data
--------------

The data consists of two months of data from an anonymous individual
collected during the months of October and November, 2012 and include
the number of steps taken in 5 minute intervals each day.

**Dataset**: [Activity monitoring data
\[52K\]](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

-   **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as **NA**).
-   **date**: The date on which the measurement was taken in
    **YYYY-MM-DD** format.
-   **interval**: Identifier for the **5-minute** interval in which
    measurement was taken.

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of **17,568** observations in this dataset.

<p align="center">
<img src="https://xmnuz23762.i.lithium.com/t5/image/serverpage/image-id/29301i0133060A68A0D056?v=1.0">
</p>  

Here we go!
-----------

Let's start by loading the dataset and taking a look at its structure.

    data <- read.csv(unzip("activity.zip"))
    str(data)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

As you can see, the **date** column is a *Factor* vector. Better we
transform it into a *Date* vector.

    data$date <- as.Date(data$date)
    str(data)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

It can be noticed that at least the first ten entries of **steps** are
missing. It would be interesting look at the proportion of *NAs* but by
now we'll keep the focus on answer some questions.

What is mean total number of steps taken per day?
-------------------------------------------------

To answer that, we need to calculate first the total number of steps
taken per day. *tapply* looks like the right tool for this.

    perday <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)
    str(perday)

    ##  int [1:61(1d)] 0 126 11352 12116 13294 15420 11015 0 12811 9900 ...
    ##  - attr(*, "dimnames")=List of 1
    ##   ..$ : chr [1:61] "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" ...

Let's visualize it. We'll make a **histogram** of the total number of
steps taken per day.

    library(ggplot2)

    qplot(perday,
          binwidth = 1000,
          xlab = "Total number of steps taken per day", 
          ylab = "Frequency", 
          main = "Histogram: Total number of steps taken per day")

![](figure/plot-1.png)

It seems like our mate is a little lazy! We are not, let's move onto
some *central tendency* measurements.

    mean(perday)

    ## [1] 9354.23

    median(perday)

    ## [1] 10395

So, answering the question, the **mean** total number of steps taken per
day is **9354.23**.  
We're ready for the next question.

What is the average daily activity pattern?
-------------------------------------------

To answer that we must to calculate the average steps taken by interval.
*aggregate* will do the job.

    avg <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)
    head(avg)

    ##   interval     steps
    ## 1        0 1.7169811
    ## 2        5 0.3396226
    ## 3       10 0.1320755
    ## 4       15 0.1509434
    ## 5       20 0.0754717
    ## 6       25 2.0943396

To visualize the average daily activity pattern, we'll make a **time
series plot** of the 5-minute interval (x-axis) and the average number
of steps taken, averaged across all days (y-axis).

    ggplot(avg, 
           aes(x = interval, y = steps)) +
           geom_line() +
           xlab("5min Interval") +
           ylab("Average number of steps taken") +
           ggtitle("Time Series: Average number of steps taken")

![](figure/plot-2.png)

We can see a **peak** between the intervals 500 and 1000. This peak
represent the **maximum number of steps taken on average across all the
days**.  
Since this information is critical, we'll find which exactly 5-minute
interval contains this peak.

    avg[which.max(avg$steps), 'interval']

    ## [1] 835

The interval **835** contains the maximum number of steps taken on
average across all the days.  
Now we'll back to the *NAs* issue.

Dealing with missing values
---------------------------

Since **missing values may introduce bias** into some calculations or
summaries of the data, we better consider them into our analysis.  
Let's calculate the total number of missing values in the dataset.

    sum(is.na(data$steps))

    ## [1] 2304

We report **2304** missing values, which represents **13.11%** of the
data.  
This percentage is highly enough to be considered. We'll must devise a
strategy for filling in all of the missing values in the dataset.

A **simple strategy** could be replace *NAs* with the mean value of its
5-minute interval.  
Let's code a **fill** function that will allow us to apply our strategy.

    fill <- function(steps, interval) {
      filled <- NULL
      if (!is.na(steps))
        filled <- c(steps) 
      else
        filled <- (avg[avg$interval == interval, 'steps'])
      
      filled
    }

Now we'll create a new dataset that is equal to the original but with
the missing data filled in.

    newdata <- data
    newdata$steps <- mapply(fill, newdata$steps, newdata$interval)
    head(newdata)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

To evaluate the impact of imputing missing data on the estimates of the
total daily number of steps, we'll repeat the first part of this
analysis using the new dataset, the one with *NAs* filled.

    perday <- tapply(newdata$steps, newdata$date, FUN = sum, na.rm = TRUE)

    qplot(perday,
          binwidth = 1000,
          xlab = "Total number of steps taken per day", 
          ylab = "Frequency", 
          main = "Histogram: Total number of steps taken per day")

![](figure/plot-3.png)

It looks like our mate is not lazy at all! Our judgement may have been
biased by missing values. What a shame!

    mean(perday)

    ## [1] 10766.19

    median(perday)

    ## [1] 10766.19

Indeed, **the mean total number of steps taken per day** increase from
**9354.23** to **10766.19**.  
Once the *NAs* issue was solved, we'll move onto the next and final
question.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

We'll add a new factor variable in the dataset with two levels
(“weekday” and “weekend”) indicating whether a given date is a
**weekday** or **weekend** day.

felse(as.POSIXlt(activityDataImputed*d**a**t**e*)wday %in% c(0,6),
'weekend', 'weekday')

    newdata$day <- ifelse(as.POSIXlt(newdata$date)$wday %in% c(0,6),
                          'weekend', 'weekday')
    head(newdata)

    ##       steps       date interval     day
    ## 1 1.7169811 2012-10-01        0 weekday
    ## 2 0.3396226 2012-10-01        5 weekday
    ## 3 0.1320755 2012-10-01       10 weekday
    ## 4 0.1509434 2012-10-01       15 weekday
    ## 5 0.0754717 2012-10-01       20 weekday
    ## 6 2.0943396 2012-10-01       25 weekday

Now we are ready to answer the question in a visual way. Let's make a
**panel plot containing two time series plots** of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all
weekday days or weekend days (y-axis).

    avg <- aggregate(steps ~ interval + day, newdata, mean)

    ggplot(avg, 
           aes(interval, steps)) +
           geom_line() +
           facet_grid(day ~ .)  +
           xlab("5min Interval") +
           ylab("Average number of steps taken") +
           ggtitle("Time Series: Average number of steps taken")

![](figure/plot-4.png)

We can see how in several intervals the average number of steps taken
**increase**.  
At first sight, it seems that our mate is more active during weekends.

Conclusions
-----------

We saw how useful are **histograms** and **time series plots** to get
some insights while performing exploratory analysis with data collected
by personal activity monitoring devices.

Since **missing values could bias our analysis**, they deserve special
attention.
