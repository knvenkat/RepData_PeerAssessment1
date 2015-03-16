# Reproducible Research - Peer Assessment 1
KNVENKAT (repdata-012)  

### Loading and pre-processing data

Checking and loading data. In this step, validation is performed on souce location of data and cleaned of missing values

*Steps with 'NA' are filtered out*

Loading supporting packages

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```r
if (!file.exists("activity.csv"))
{ stop ("Source file not present in working directory")}

# Read data and convert date column from factor into data and remove steps with NA
act <- read.csv("activity.csv")
act$date <- as.Date(act$date, "%Y-%m-%d")
a <- subset(act, !is.na(act$steps))
```

### What is the mean total number of steps taken per day?

Group the data by dates and calculate the total number to steps taken per day. Ensure to assign valid column names at all stages in order to make reference easy in future steps.



```r
grp_a <- group_by(a, date)
sum_a <- summarize(grp_a, sum(steps))
colnames(sum_a) <- c("date", "total")
hist(sum_a$total, main="Histogram of steps for all days", xlab="")
```

![](Markdown1_files/figure-html/unnamed-chunk-3-1.png) 

Calculate other related measures are median and mean and consolidate into a single data frame.

```r
# claculate mean and median of total steps per day
mean_a <- summarize(grp_a, mean(steps))
colnames(mean_a) <- c("date", "avg")
median_a <- summarize(grp_a, median(steps))
colnames(median_a) <- c("date", "median")

# create final output
final_output <- merge(sum_a, mean_a, by.x = "date", by.y = "date")
final_output <- merge(final_output, median_a, by.x = "date", by.y = "date")
```
Examine some rows of the final output generated for reference

```r
head(final_output,5); tail(final_output,5); summary(final_output)
```

```
##         date total      avg median
## 1 2012-10-02   126  0.43750      0
## 2 2012-10-03 11352 39.41667      0
## 3 2012-10-04 12116 42.06944      0
## 4 2012-10-05 13294 46.15972      0
## 5 2012-10-06 15420 53.54167      0
```

```
##          date total      avg median
## 49 2012-11-25 11834 41.09028      0
## 50 2012-11-26 11162 38.75694      0
## 51 2012-11-27 13646 47.38194      0
## 52 2012-11-28 10183 35.35764      0
## 53 2012-11-29  7047 24.46875      0
```

```
##       date                total            avg              median 
##  Min.   :2012-10-02   Min.   :   41   Min.   : 0.1424   Min.   :0  
##  1st Qu.:2012-10-16   1st Qu.: 8841   1st Qu.:30.6979   1st Qu.:0  
##  Median :2012-10-29   Median :10765   Median :37.3785   Median :0  
##  Mean   :2012-10-30   Mean   :10766   Mean   :37.3826   Mean   :0  
##  3rd Qu.:2012-11-16   3rd Qu.:13294   3rd Qu.:46.1597   3rd Qu.:0  
##  Max.   :2012-11-29   Max.   :21194   Max.   :73.5903   Max.   :0
```
#####*Notice that the median values are zero when computed in this manner*

### What is the average daily pattern?
Summarize the data according the interval and calculate the average of steps walked across all days. Draw a line graph to show the average steps taken against interval. 

#####NOTE: NA Values are removed in order to keep the values consistent. The function will return NA for all intervals calculated

```r
plot_data <- group_by(act, interval)
plot_data <- summarize(plot_data, mean(steps, na.rm=TRUE))
colnames(plot_data) <- c("interval", "avg")
plot(plot_data$interval, plot_data$avg, type="l", main="Interval versus Average Steps", xlab="Interval", ylab = "Average Steps")
```

![](Markdown1_files/figure-html/unnamed-chunk-6-1.png) 

#####_It looks like the interval with maximum average steps falls around 800 range._

### Imputing missing values

How many rows have 'NA' values in the dataset?

```
## [1] 2304
```

Let us fill the missing values with mean computed for entire date and update the original dataset. Try to find average by date and if that fails, try to find average by interval and when both attempts fail, set the value as zero.


```r
for(i in 1:nrow(act))
{
  # check for average based on date
  if(is.na(act[i,1]))
  {
     tmp <- subset(act, act[,2] == act[i,2])
     tmp_mean <- mean(tmp[,1], na.rm = TRUE)
     
     #check for average based on interval
     if (is.na(tmp_mean) | tmp_mean == 0)
      {
        tmp <- subset(act, act[,3] == act[i,3])
        tmp_mean <- mean(tmp[,1], na.rm = TRUE)
        if (is.na(tmp_mean))
        {
          # assign zero if both method fail
          tmp_mean = 0
        }
      }
     act[i,1] <- tmp_mean
  }
}
# view summary of modified dataset
summary(act)
```

```
##      steps              date               interval     
##  Min.   :  0.000   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.000   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.000   Median :2012-10-31   Median :1177.5  
##  Mean   : 32.705   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.:  1.717   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.000   Max.   :2012-11-30   Max.   :2355.0
```
NOTE: The values have drastically changes for "steps" variable

Build the remining summary dataset and plot histogram of modified values

```r
grp_act <- group_by(act, date)
sum_act <- summarize(grp_act, sum(steps))
colnames(sum_act) <- c("date", "total")
hist(sum_act$total, main="Histogram of steps for all days - derived values", xlab="")
```

![](Markdown1_files/figure-html/unnamed-chunk-9-1.png) 

#### NOTE: New histogram differs drastically from the original plot.


```r
mean_act <- summarize(grp_act, mean(steps))
colnames(mean_act) <- c("date", "avg")
median_act <- summarize(grp_act, median(steps))
colnames(median_act) <- c("date", "median")

#output final
final_output_mod <- merge(sum_act, mean_act, by.x = "date", by.y = "date")
final_output_mod <- merge(final_output_mod, median_act, by.x = "date", by.y = "date")

summary(final_output_mod)
```

```
##       date                total            avg              median      
##  Min.   :2012-10-01   Min.   :   41   Min.   : 0.1424   Min.   :0.0000  
##  1st Qu.:2012-10-16   1st Qu.: 6778   1st Qu.:23.5347   1st Qu.:0.0000  
##  Median :2012-10-31   Median :10395   Median :36.0938   Median :0.0000  
##  Mean   :2012-10-31   Mean   : 9419   Mean   :32.7051   Mean   :0.2252  
##  3rd Qu.:2012-11-15   3rd Qu.:12811   3rd Qu.:44.4826   3rd Qu.:0.0000  
##  Max.   :2012-11-30   Max.   :21194   Max.   :73.5903   Max.   :1.7170
```

###Number of observations prior to imputing = 53
###Number of observations after imputing = 61

###Are there difference between activity patterns between weekdays and weekends?

Add a new factor varaible to the data frame by checkcing whether the weekday of the date variabe. Plot the graph between these two data points. 

```r
act$day_flag <- as.factor(ifelse((weekdays(act$date) == "Sunday")|(weekdays(act$date) == "Saturday"),"Y","N"))

qplot(act$interval, act$steps, geom="line", colour=act$day_flag, xlab="Interval", ylab="Steps", main="Weekend vs Weekday steps taken for adjusted data")
```

![](Markdown1_files/figure-html/unnamed-chunk-11-1.png) 

We can observe that walking patterns change during the day of week.
