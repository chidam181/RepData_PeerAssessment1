---
title: "Assignment1_RR"
output: html_document
date: "23-04-2017"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Load libraries and set working directory

```{r Load libraries}
# Load libraries and set working directory
setwd("C://Users//Nishanth//Desktop//Learning R programing")
library(knitr)
library(ggplot2)
library(lattice)
```


## Loading and preprocessing the data

The following code unzips the zip file and loads the csv file to a variable named data. The NA's are removed and saved to data_final. Finally the data is checked
```{r Loading and preprocessing the data}
## Unzip, load, clean and check data
unzip(zipfile = "activity.zip")
data <- read.csv("activity.csv")
data_final <- data[ with (data, { !(is.na(steps)) } ), ]
str(data)
str(data_final)
```

## What is mean total number of steps taken per day?

The following steps plots the histogram

```{r What is mean total number of steps taken per day?}
stepsPD <- tapply(data_final$steps, data_final$date, sum)
```
```{r, echo=TRUE}
hist(stepsPD,xlab="Total number of steps in a day")
```

### Calculate and report the mean and median total number of steps taken per day

The following steps calculates the mean and median of the data. The mean is **10766.19** and median is **10765**

```{r, echo=TRUE, eval=TRUE}
stepsPD_mean <- mean(stepsPD)
stepsPD_median <- median(stepsPD)
```

## What is the average daily activity pattern?
For steps of 5 minute intervals the following code is used to calculate
```{r}
stepsPI <- aggregate(steps ~ interval, data, mean)
plot(stepsPI$interval, stepsPI$steps, type ='l', xlab="Interval", 
     ylab="Average number of steps")
max_interval <- stepsPI[which.max(stepsPI$steps),]
```

## Imputing missing values

### The number of missing na's is **2304**
```{r}
sum(is.na(data))   
```

### Replacing the NA's
For replacing the NA's the mean value at the same interval across days is used. The following code replaces these from the initial dataset which doesn't have its NA's removed. Finally, checking if the sizes are the same as that of the intial set.
```{r}
data_replace <- data
for (i in 1:nrow(data_replace)) {
        if (is.na(data_replace$steps[i])) {interval_value <- data_replace$interval[i]
                steps_value <- stepsPI[stepsPI$interval == interval_value,]
                data_replace$steps[i] <- steps_value$steps
        }
}
str(data_replace)
```
### Calculating new mean and medians and plotting the new histogram

```{r}
stepsPI_replace <- tapply(data_replace$steps, data_replace$date, sum)
hist(stepsPI_replace)
stepsPD_mean_Replace <- mean(stepsPI_replace)
stepsPD_median_Replace <- median(stepsPI_replace)
```
## Are there differences in activity patterns between weekdays and weekends?
From the plots obtained below we can see a clear difference in the patterns for weekends and weekdays. This is most probably owing to the fact of daily work-home pattern on weekdays.
```{r}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
data_replace$day = as.factor(ifelse(is.element(weekdays(as.Date(data_replace$date)),weekdays), 
                                    "Weekday", "Weekend"))
stepsPI_day<- aggregate(steps ~ interval + day, data_replace, mean)
xyplot(stepsPI_day$steps ~ stepsPI_day$interval|stepsPI_day$day, 
       main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```
