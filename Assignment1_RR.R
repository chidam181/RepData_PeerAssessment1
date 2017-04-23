# Load libraries and set working directory
setwd("C://Users//Nishanth//Desktop//Learning R programing")
library(knitr)
library(ggplot2)
library(lattice)
## Unzip, load, clean and check data
unzip(zipfile = "activity.zip")
data <- read.csv("activity.csv")
data_final <- data[ with (data, { !(is.na(steps)) } ), ]
data_final$date <- as.Date(data_final$date, format = "%Y-%m-%d")
data_final$interval <- as.factor(data_final$interval)
str(data_final)
## Calculating mean, median and plotting histogram
stepsPD <- tapply(data_final$steps, data_final$date, sum)
hist(stepsPD,xlab="Total number of steps in a day")
stepsPD_mean <- mean(stepsPD)
stepsPD_mean
stepsPD_median <- median(stepsPD)
stepsPD_median
## Calculating average daily activity pattern
stepsPI <- aggregate(steps ~ interval, data, mean)
plot(stepsPI$interval, stepsPI$steps, type ='l', xlab="Interval", 
     ylab="Average number of steps")
max_interval <- stepsPI[which.max(stepsPI$steps),]
## Imputing missing values
sum(is.na(data))           # Number of missing data
# Replace the na's with mean data and plot historgram
data_replace <- data
for (i in 1:nrow(data_replace)) {
        if (is.na(data_replace$steps[i])) {interval_value <- data_replace$interval[i]
                steps_value <- stepsPI[stepsPI$interval == interval_value,]
                data_replace$steps[i] <- steps_value$steps
        }
}
stepsPI_replace <- tapply(data_replace$steps, data_replace$date, sum)
hist(stepsPI_replace)
stepsPD_mean_Replace <- mean(stepsPI_replace)
stepsPD_median_Replace <- median(stepsPI_replace)
## Difference between weekend and weekday
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
data_replace$day = as.factor(ifelse(is.element(weekdays(as.Date(data_replace$date)),weekdays), 
                                    "Weekday", "Weekend"))
stepsPI_day<- aggregate(steps ~ interval + day, data_replace, mean)
xyplot(stepsPI_day$steps ~ stepsPI_day$interval|stepsPI_day$day, 
       main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
