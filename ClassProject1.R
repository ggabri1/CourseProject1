#################### === Class Project === ###########################
#### Loading and preprocessing the data
## 1. Code for reading in the dataset and/or processing the data  
library(ggplot2)
library(stats)
## setwd("./Classes/Reproducible Data/ClassProject1")
dataset_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(dataset_url, "samsung.zip")
unzip("samsung.zip")
list.files() #Getting the list of the files in subfolder for the data

#== 1. Loading the data
activity <- read.csv("./activity.csv")
head(activity)

#== 2. Process/transform the data (if necessary) into a format suitable for your analysis
## removing missing observations 
activity_nomis <- activity[complete.cases(activity), ]


#== What is mean total number of steps taken per day?
#== 1. Calculating the total number of steps taken per day

# Summming steps across days
steps_day <- aggregate(steps ~ date, activity_nomis, sum)
head(steps_day)

## 2. Histogram of the total number of steps taken each day
#Plotting the histogram
hist(steps_day$steps, main = "The total number of steps taken each day", xlab = "Steps by Day")

#== 3. Calculating and reporting the mean and median of the total number of steps taken per day
## 3. Mean and median number of steps taken each day
summary(steps_day$steps)

##Reporting the meand and the median numbers seperately
round(mean(steps_day$steps))
median(steps_day$steps)

#== What is the average daily activity pattern?
#== 1. Make a time series plot (i.e.  type = "l") of the 5-minute interval
#==  (x-axis) and the average number of steps taken, averaged across all days (y-axis)


## 4. Time series plot of the average number of steps taken
# Calculating average number of steps taken averaged across all days
steps_day_average <- aggregate(steps ~ interval, activity_nomis, FUN = "mean")
head(steps_day_average)


# Plotting time series of the average number of steps taken
plot(steps_day_average$interval, steps_day_average$steps, type ="l", 
     main = "Time series plot of the average number of steps taken", 
     xlab = "5-minute Intervals", ylab = "Average Number of Steps")

#== Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

## 5. The 5-minute interval that, on average, contains the maximum number of steps
# Finding the intervals that include the maximum values
interval_max <- which.max(steps_day_average$steps)

# The interval that contains the maximum number of steps
steps_day_average[interval_max, ]$interval
# and the number of steps for that interval is
round(steps_day_average [interval_max, ]$steps, digits = 1)



#== Imputing missing values
#== 1. Calculate and report the total number of missing values in the dataset 
#== (i.e. the total number of rows with NAs)
## 6. Code to describe and show a strategy for imputing missing data
# Identify missing observations
activity_mis <- activity[!complete.cases(activity), ]
dim(activity_mis)
nrow(activity_mis)


#== 2. Devise a strategy for filling in all of the missing values in the dataset. 
#== The strategy does not need to be sophisticated. For example, you could use 
#== the mean/median for that day, or the mean for that 5-minute interval, etc.
# I use mean values for steps taken in each interval to replace missing observations. 

#== Create a new dataset that is equal to the original dataset but with the missing data filled in. 
# Make a for loop to replace all missing values with respective average values by intervals
for (i in 1:nrow(activity)) {
        if(is.na(activity$steps[i])) {
                steps_interval <- steps_day_average$steps[which(steps_day_average$interval == activity$interval[i])]
                activity$steps[i] <- steps_interval 
        }
}

# Checking the actual data 
head(activity)
dim(activity)
#== Make a histogram of the total number of steps taken each day and Calculate and report 
#== the mean and median total number of steps taken per day. Do these values differ from 
#== the estimates from the first part of the assignment? What is the impact of imputing 
#== missing data on the estimates of the total daily number of steps?

# Reporting the mean and the median of imputed data
steps_day_replaced <- aggregate(steps ~ date, activity, sum)
head(steps_day_replaced)

round(mean(steps_day_replaced$steps))
median(steps_day_replaced$steps)


## 7. Histogram of the total number of steps taken each day after missing values are imputed
#Plotting the histogram
hist(steps_day_replaced$steps, main = "The total number of steps taken each day 
     with Imputed values", xlab = "Steps by Day")


## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# Identifying the dates that correspond to weekends or weekdays
weekday <- function(dates) {
        week_days <- weekdays(as.Date(dates, '%Y-%m-%d'))
        if  (!(week_days == 'Saturday' || week_days == 'Sunday')) {
                x <- 'Weekday'
        } else {
                x <- 'Weekend'
        }
        x
}

# Adding newly created weekdays to the data frame
activity$days <- as.factor(sapply(activity$date, weekday))
steps_day_replaced_new <- aggregate(steps ~ interval+days, activity, FUN = "mean")

head(steps_day_replaced_new) 

## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
g <- ggplot(steps_day_replaced_new, aes(x=interval, y=steps, fill =days))
g +     geom_line(stat = "identity", aes(color = days)) +
        facet_grid(days ~., scales = "fixed",space="fixed") +
        labs(y = "Average Steps in Interval") + 
        labs(x = "Intervals") +
        labs(title = "The average number of steps taken per 5-minute interval across weekdays and weekends") 
